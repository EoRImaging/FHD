FUNCTION beam_setup,obs,antenna,file_path_fhd=file_path_fhd,restore_last=restore_last,timing=timing,$
    residual_tolerance=residual_tolerance,residual_threshold=residual_threshold,beam_mask_threshold=beam_mask_threshold,$
    silent=silent,psf_dim=psf_dim,psf_resolution=psf_resolution,psf_image_resolution=psf_image_resolution,$
    swap_pol=swap_pol,no_complex_beam=no_complex_beam,no_save=no_save,beam_pol_test=beam_pol_test,$
    psf_max_dim=psf_max_dim,beam_model_version=beam_model_version,_Extra=extra

compile_opt idl2,strictarrsubs  
t00=Systime(1)

IF Keyword_Set(restore_last) AND (file_test(file_path_fhd+'_beams'+'.sav') EQ 0) THEN BEGIN 
    IF not Keyword_Set(silent) THEN print,file_path_fhd+'_beams'+'.sav' +' Not found. Recalculating.' 
    restore_last=0
ENDIF
IF Keyword_Set(restore_last) THEN BEGIN
    IF not Keyword_Set(silent) THEN print,'Saved beam model restored'
    RESTORE,file_path_fhd+'_beams'+'.sav' ;psf,antenna
    RETURN,psf
ENDIF

IF Tag_exist(obs,'instrument') THEN instrument=obs.instrument ELSE instrument='mwa'
tile_beam_fn=instrument+'_tile_beam_generate' ;mwa_tile_beam_generate
tile_gain_fn=instrument+'_beam_setup_init' ;mwa_beam_setup_init
tile_mask_fn=instrument+'_tile_beam_mask' ;mwa_tile_beam_mask
IF instrument EQ 'paper' THEN base_gain=fltarr(1)+1.
;Fixed parameters 
IF N_Elements(obs) EQ 0 THEN RESTORE,file_path+'_obs.sav'
;extract information from the structures
n_tiles=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol

obsra=obs.obsra
obsdec=obs.obsdec
zenra=obs.zenra
zendec=obs.zendec
phasera=obs.phasera
phasedec=obs.phasedec
Jdate=obs.Jd0
frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
bin_offset=(*obs.baseline_info).bin_offset
nbaselines=bin_offset[1]

dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
degpix=obs.degpix
astr=obs.astr

antenna=fhd_struct_init_antenna(obs,file_path_fhd=file_path_fhd,beam_model_version=beam_model_version,$
    psf_resolution=psf_resolution,psf_image_resolution=psf_image_resolution,timing=t_ant,_Extra=extra)

IF tag_exist(obs,'delays') THEN delay_settings=obs.delays
;IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.

IF Keyword_Set(swap_pol) THEN pol_arr=[[1,1],[0,0],[1,0],[0,1]] ELSE pol_arr=[[0,0],[1,1],[0,1],[1,0]] 
speed_light=299792458. ;speed of light, in meters/second
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32? ;super-resolution factor
;IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=10.
Eq2Hor,obsra,obsdec,Jdate,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
obsalt=Float(obsalt)
obsaz=Float(obsaz)
obsza=90.-obsalt
psf_dim=Ceil((Max(antenna.size_meters)*2.*Max(frequency_array)/speed_light)/kbinsize/Cos(obsza*!DtoR))  
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even

;residual_tolerance is residual as fraction of psf_base above which to include 
IF N_Elements(residual_tolerance) EQ 0 THEN residual_tolerance=1./100.  
;residual_threshold is minimum residual above which to include
IF N_Elements(residual_threshold) EQ 0 THEN residual_threshold=0.
IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=1E3

;freq_center=fltarr(nfreq_bin)
;FOR fi=0L,nfreq_bin-1 DO BEGIN
;    fi_i=where(freq_bin_i EQ fi,n_fi)
;    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) $
;        ELSE freq_center[fi]=Median(frequency_array[fi_i])
;ENDFOR

;;freq_norm=freq_center^(-alpha)
;;;freq_norm/=Sqrt(Mean(freq_norm^2.))
;;freq_norm/=Mean(freq_norm) 
freq_norm=Replicate(1.,nfreq_bin)

;begin forming psf
psf_residuals_i=Ptrarr(n_pol,nfreq_bin,nbaselines) ;contains arrays of pixel indices of pixels with modified psf for a given baseline id
psf_residuals_val=Ptrarr(n_pol,nfreq_bin,nbaselines) ;contains arrays of values corresponding to the pixel indices above
psf_residuals_n=fltarr(n_pol,nfreq_bin,nbaselines) ;contains the total number of modified pixels for each baseline id
psf_base=Ptrarr(n_pol,nfreq_bin,psf_resolution,psf_resolution)

psf_xvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
psf_yvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
xvals_i=meshgrid(psf_dim,psf_dim,1)*psf_resolution
yvals_i=meshgrid(psf_dim,psf_dim,2)*psf_resolution
FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN 
    *psf_xvals[i,j]=meshgrid(psf_dim,psf_dim,1)-psf_dim/2.+Float(i)/psf_resolution
    *psf_yvals[i,j]=meshgrid(psf_dim,psf_dim,2)-psf_dim/2.+Float(j)/psf_resolution
ENDFOR

t1_a=Systime(1)
;;set up coordinates to generate the high uv resolution model. 
;;Remember that field of view = uv resolution, image pixel scale = uv span. 
;;So, the cropped uv span (psf_dim) means we do not need to calculate at full image resolution, 
;;   while the increased uv resolution can correspond to super-horizon scales. We construct the beam model in image space, 
;;   and while we don't need the full image resolution we need to avoid quantization errors that come in if we make too small an image and then take the FFT
;psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
;psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model
;psf_superres_dim=psf_dim*psf_resolution
;psf_scale=dimension*psf_intermediate_res/psf_image_dim
;xvals_celestial=meshgrid(psf_image_dim,psf_image_dim,1)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
;yvals_celestial=meshgrid(psf_image_dim,psf_image_dim,2)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
;xvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,1)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
;yvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,2)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
;
;xy2ad,xvals_celestial,yvals_celestial,astr,ra_arr,dec_arr
;valid_i=where(Finite(ra_arr),n_valid)
;ra_use=ra_arr[valid_i]
;dec_use=dec_arr[valid_i]

;;NOTE: Eq2Hor REQUIRES Jdate to have the same number of elements as RA and Dec for precession!!
;;;NOTE: The NEW Eq2Hor REQUIRES Jdate to be a scalar! They created a new bug when they fixed the old one
;Eq2Hor,ra_use,dec_use,Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
;za_arr=fltarr(psf_image_dim,psf_image_dim)+90. & za_arr[valid_i]=90.-alt_arr1
;az_arr=fltarr(psf_image_dim,psf_image_dim) & az_arr[valid_i]=az_arr1
;
;xvals_instrument=za_arr*Sin(az_arr*!DtoR)
;yvals_instrument=za_arr*Cos(az_arr*!DtoR)

;hour_angle=obs.obsra - ra_use
;h_neg = where(hour_angle LT 0, N_neg)
;IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
;hour_angle = hour_angle mod 360.
;hadec2altaz, hour_angle, dec_use, obs.obsdec, elevation_use, azimuth_use
;elevation_arr=fltarr(psf_image_dim,psf_image_dim) & elevation_arr[valid_i]=elevation_use
;azimuth_arr=fltarr(psf_image_dim,psf_image_dim) & azimuth_arr[valid_i]=azimuth_use

norm=[sqrt(1.-(sin(obsza*!DtoR)*sin((obsaz)*!DtoR))^2.),sqrt(1.-(sin(obsza*!DtoR)*cos((obsaz)*!DtoR))^2.)]
pol_norm=fltarr(n_pol)+1.
FOR pol_i=0,n_pol-1 DO pol_norm[pol_i]=(norm[pol_arr[0,pol_i]]*norm[pol_arr[1,pol_i]])

t1=Systime(1)-t1_a
t2=0
t3=0
t4=0

complex_flag_arr=intarr(n_pol,nfreq_bin)
FOR pol_i=0,n_pol-1 DO BEGIN

    pol1=pol_arr[0,pol_i]
    pol2=pol_arr[1,pol_i]
    freq_norm_check=fltarr(nfreq_bin)+1.
    
    FOR freq_i=0,nfreq_bin-1 DO BEGIN        
        t2_a=Systime(1)
        antenna_beam_arr1=Ptrarr(16,/allocate)
        antenna_beam_arr2=Ptrarr(16,/allocate)
        beam1_arr=Ptrarr(n_tiles,/allocate)
        beam2_arr=Ptrarr(n_tiles,/allocate)
        
        gain1=Reform((*gain_arr[pol1])[freq_i,*,*])
        gain2=Reform((*gain_arr[pol2])[freq_i,*,*])
        
        gain1_avg=Median(gain1,dimension=1)
        gain2_avg=Median(gain2,dimension=1)
        
        ;mwa_tile_beam_generate.pro paper_tile_beam_generate.pro
        IF Keyword_Set(mutual_coupling) THEN BEGIN
            mutual_coupling1=*mutual_coupling[pol1,freq_i]
            mutual_coupling2=*mutual_coupling[pol2,freq_i]
        ENDIF
        beam1_0=Call_function(tile_beam_fn,gain1_avg,antenna_beam_arr1,$ ;mwa_tile_beam_generate
            frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals_instrument,yvals=yvals_instrument,$
            ra_arr=ra_arr,dec_arr=dec_arr,delay_settings=delay_settings,dimension=psf_image_dim,$
            beam_model_version=beam_model_version,mutual_coupling=mutual_coupling1)
;        IF pol2 EQ pol1 THEN antenna_beam_arr2=antenna_beam_arr1
        beam2_0=Call_function(tile_beam_fn,gain2_avg,antenna_beam_arr2,$ ;mwa_tile_beam_generate
            frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals_instrument,yvals=yvals_instrument,$
            ra_arr=ra_arr,dec_arr=dec_arr,delay_settings=delay_settings,dimension=psf_image_dim,$
            beam_model_version=beam_model_version,mutual_coupling=mutual_coupling2)
        Ptr_free,antenna_beam_arr1,antenna_beam_arr2
        t3_a=Systime(1)
        t2+=t3_a-t2_a
        ;FFT individual tile beams to uv space, crop there, and FFT back
        beam1_0=mask_beam(obs,antenna[ant_1],beam1_0,psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,freq=freq_center[freq_i]) 
        beam2_0=mask_beam(obs,antenna[ant_2],beam2_0,psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,freq=freq_center[freq_i]) 
        
        psf_base_single=dirty_image_generate(beam1_0*Conj(beam2_0),/no_real)
        psf_base_superres=Interpolate(psf_base_single,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)
        psf_base_superres*=psf_intermediate_res^2. ;FFT normalization correction in case this changes the total number of pixels
;        phase_test=Atan(psf_base_superres,/phase)*!Radeg
;        phase_cut=where(Abs(phase_test) GE 90.,n_phase_cut)
;        IF n_phase_cut GT 0 THEN uv_mask_superres[phase_cut]=0
        freq_norm_check[freq_i]=Total(Abs(psf_base_superres))/psf_resolution^2.
        gain_normalization=1./(Total(Abs(psf_base_superres))/psf_resolution^2.)
;        psf_base_superres*=gain_normalization
;        psf_base_superres*=freq_norm[freq_i]
;        psf_base_superres*=pol_norm[pol_i]
        t4_a=Systime(1)
        t3+=t4_a-t3_a
        phase_mag=(Abs(Atan(psf_base_superres,/phase))<Abs(!Pi-Abs(Atan(psf_base_superres,/phase))))*Floor(uv_mask_superres>0)
        IF Max(phase_mag) GT !Pi*residual_tolerance THEN complex_flag_arr[pol_i,freq_i]=1
        
;        FOR tile_i=0,n_tiles-1 DO BEGIN
;            *beam1_arr[tile_i]=Call_function(tile_beam_fn,gain1[*,tile_i],antenna_beam_arr1,$
;                frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals_instrument,yvals=yvals_instrument,$
;                ra_arr=ra_arr,dec_arr=dec_arr,delay_settings=delay_settings,dimension=dimension)
;            *beam2_arr[tile_i]=Call_function(tile_beam_fn,gain2[*,tile_i],antenna_beam_arr2,$
;                frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals_instrument,yvals=yvals_instrument,$
;                ra_arr=ra_arr,dec_arr=dec_arr,delay_settings=delay_settings,dimension=dimension)
;        ENDFOR
;        
;        FOR bi=0,nbaselines-1 DO BEGIN
;            IF Min((gain1[*,tile_A[bi]-1]-gain1_avg EQ fltarr(N_Elements(base_gain))) $
;                AND (gain2[*,tile_B[bi]-1]-gain2_avg EQ fltarr(N_Elements(base_gain)))) THEN BEGIN
    ;                psf_residuals_n[pol_i,freq_i,bi]=0
    ;                CONTINUE
;            ENDIF
;            
;            psf_single=dirty_image_generate(*beam1_arr[tile_A[bi]-1],*beam2_arr[tile_B[bi]-1])*uv_mask*gain_normalization
;            residual_single=psf_single-psf_base_superres
;            i_res=where(residual_single GE ((psf_base_superres*residual_tolerance)>residual_threshold),nres)
;            psf_residuals_n[pol_i,freq_i,bi]=nres
;            IF nres GT 0 THEN BEGIN
;                psf_residuals_i[pol_i,freq_i,bi]=Ptr_new(i_res)
;                psf_residuals_val[pol_i,freq_i,bi]=Ptr_new(residual_single[i_res])
;            ENDIF
;        ENDFOR
        Ptr_free,antenna_beam_arr1,antenna_beam_arr2,beam1_arr,beam2_arr
        FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
            psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j]=$
                Ptr_new(psf_base_superres[xvals_i+i,yvals_i+j]) 
        breakpoint0=0
        t4+=Systime(1)-t4_a
    ENDFOR
    freq_norm_check/=mean(freq_norm_check)
;    FOR freq_i=0L,nfreq_bin-1 DO FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
;        *psf_base[pol_i,freq_i,i,j]/=freq_norm_check[freq_i]
ENDFOR

;higher than necessary psf_dim is VERY computationally expensive, but we also don't want to crop the beam if there is real signal
;   So, in case a larger than necessary psf_dim was specified above, reduce it now if that is safe
edge_test=fltarr(psf_dim,psf_dim)
FOR i=0,N_Elements(psf_base)-1 DO edge_test+=Abs(*psf_base[i]) ;add together ALL beams, because we just want to find out if EVERY border pixel is zero
edge_test_cut=Total(edge_test,1)+Total(edge_test,2) 
edge_test_cut+=Reverse(edge_test_cut)
edge_zeroes=(where(edge_test_cut))[0]
IF Keyword_set(psf_max_dim) THEN edge_zeroes=edge_zeroes>Round((psf_dim-psf_max_dim)/2)
IF edge_zeroes GT 0 THEN BEGIN
    psf_dim-=2.*edge_zeroes
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
        *psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j]=$
            Reform((*psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j])[edge_zeroes:edge_zeroes+psf_dim-1,edge_zeroes:edge_zeroes+psf_dim-1],psf_dim*psf_dim)
ENDIF ELSE FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
    *psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j]=Reform(*psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j],psf_dim*psf_dim)

complex_flag=Max(complex_flag_arr)
IF Keyword_Set(no_complex_beam) THEN complex_flag=0
IF complex_flag EQ 0 THEN BEGIN
    FOR i=0L,N_Elements(psf_base)-1 DO *psf_base[i]=Real_part(*psf_base[i])
    print,'Saving only real part of beam model!'
ENDIF

t5_a=Systime(1)
psf=fhd_struct_init_psf(base=psf_base,res_i=psf_residuals_i,res_val=psf_residuals_val,$
    res_n=psf_residuals_n,xvals=psf_xvals,yvals=psf_yvals,fbin_i=freq_bin_i,$
    psf_resolution=psf_resolution,psf_dim=psf_dim,complex_flag=complex_flag,pol_norm=pol_norm,freq_norm=freq_norm,$
    n_pol=n_pol,n_freq=n_freq,freq_cen=freq_center,gain_arr=gain_arr,mutual_coupling=mutual_coupling)
IF ~Keyword_Set(no_save) THEN SAVE,psf,antenna,filename=file_path_fhd+'_beams'+'.sav',/compress
t5=Systime(1)-t5_a
timing=Systime(1)-t00
IF ~Keyword_Set(silent) THEN print,[timing,t1,t2,t3,t4,t5]
RETURN,psf
END
