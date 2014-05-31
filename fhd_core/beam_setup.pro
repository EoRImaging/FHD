FUNCTION beam_setup,obs,file_path_fhd,restore_last=restore_last,timing=timing,$
    residual_tolerance=residual_tolerance,residual_threshold=residual_threshold,$
    silent=silent,psf_dim=psf_dim,psf_resolution=psf_resolution,$
    swap_pol=swap_pol,no_complex_beam=no_complex_beam,no_save=no_save,beam_pol_test=beam_pol_test,_Extra=extra

compile_opt idl2,strictarrsubs  
t00=Systime(1)

IF Keyword_Set(restore_last) AND (file_test(file_path_fhd+'_beams'+'.sav') EQ 0) THEN BEGIN 
    IF not Keyword_Set(silent) THEN print,file_path_fhd+'_beams'+'.sav' +' Not found. Recalculating.' 
    restore_last=0
ENDIF
IF Keyword_Set(restore_last) THEN BEGIN
    IF not Keyword_Set(silent) THEN print,'Saved beam model restored'
    restore,file_path_fhd+'_beams'+'.sav'
    RETURN,psf
ENDIF

IF Tag_exist(obs,'instrument') THEN instrument=obs.instrument ELSE instrument='mwa'
tile_beam_fn=instrument+'_tile_beam_generate' ;mwa_tile_beam_generate
IF instrument EQ 'paper' THEN base_gain=fltarr(1)+1.
;Fixed parameters 
IF N_Elements(obs) EQ 0 THEN restore,file_path+'_obs.sav'
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
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq
IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
degpix=obs.degpix
IF tag_exist(obs,'delays') THEN delay_settings=obs.delays
;IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.

speed_light=299792458. ;speed of light, in meters/second
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32?
zen_ang=angle_difference(obsdec,obsra,zendec,zenra,/degree,/nearest)
IF tag_exist(obs,'antenna_size') THEN psf_dim=Ceil((obs.antenna_size*2.*Max(frequency_array)/speed_light)/kbinsize/Cos(zen_ang*!DtoR)) $
    ELSE IF N_Elements(psf_dim) EQ 0 THEN psf_dim=Ceil(2.*!Pi/kbinsize) 
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even

psf_dim2=psf_dim*psf_resolution

psf_scale=dimension/psf_dim
xvals2=meshgrid(psf_dim2,psf_dim2,1)*psf_scale-psf_dim2*psf_scale/2.+dimension/2.
yvals2=meshgrid(psf_dim2,psf_dim2,2)*psf_scale-psf_dim2*psf_scale/2.+elements/2.

;residual_tolerance is residual as fraction of psf_base above which to include 
IF N_Elements(residual_tolerance) EQ 0 THEN residual_tolerance=1./100.  
;residual_threshold is minimum residual above which to include
IF N_Elements(residual_threshold) EQ 0 THEN residual_threshold=0.
beam_mask_threshold=1E2

freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO BEGIN
    fi_i=where(freq_bin_i EQ fi,n_fi)
    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) $
        ELSE freq_center[fi]=Median(frequency_array[fi_i])
ENDFOR

;freq_norm=freq_center^(-alpha)
;;freq_norm/=Sqrt(Mean(freq_norm^2.))
;freq_norm/=Mean(freq_norm) 
freq_norm=Replicate(1.,nfreq_bin)

bin_offset=(*obs.baseline_info).bin_offset
nbaselines=bin_offset[1]

beam_setup_init,gain_array_X,gain_array_Y,file_path_fhd,n_tiles=n_tiles,nfreq_bin=nfreq_bin,base_gain=base_gain,no_save=no_save

;begin forming psf
psf_residuals_i=Ptrarr(n_pol,nfreq_bin,nbaselines) ;contains arrays of pixel indices of pixels with modified psf for a given baseline id
psf_residuals_val=Ptrarr(n_pol,nfreq_bin,nbaselines) ;contains arrays of values corresponding to the pixel indices above
psf_residuals_n=fltarr(n_pol,nfreq_bin,nbaselines) ;contains the total number of modified pixels for each baseline id

psf_base=Ptrarr(n_pol,nfreq_bin,psf_resolution,psf_resolution)
psf_xvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
psf_yvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
xvals_i=meshgrid(psf_dim,psf_dim,1)*psf_resolution
yvals_i=meshgrid(psf_dim,psf_dim,2)*psf_resolution
psf_xvals1=meshgrid(psf_dim*psf_resolution,psf_dim*psf_resolution,1)/Float(psf_resolution)-Floor(psf_dim/2)+Floor(psf_dim2/2)
psf_yvals1=meshgrid(psf_dim*psf_resolution,psf_dim*psf_resolution,2)/Float(psf_resolution)-Floor(psf_dim/2)+Floor(psf_dim2/2)
FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN 
    *psf_xvals[i,j]=meshgrid(psf_dim,psf_dim,1)-psf_dim/2.+Float(i)/psf_resolution
    *psf_yvals[i,j]=meshgrid(psf_dim,psf_dim,2)-psf_dim/2.+Float(j)/psf_resolution
ENDFOR

astr=obs.astr

t1_a=Systime(1)
xy2ad,xvals2,yvals2,astr,ra_arr2,dec_arr2
valid_i=where(Finite(ra_arr2),n_valid)
ra_use=ra_arr2[valid_i]
dec_use=dec_arr2[valid_i]

Eq2Hor,obsra,obsdec,Jdate,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
obsalt=Float(obsalt)
obsaz=Float(obsaz)
obsza=90.-obsalt
;intensity0=stokes_off_zenith(obsaz, obsalt, [1.,0.,0.,0.], Ex0, Ey0,/intensity)
;norm=Float(Sqrt(2.)*[ex0,ey0])
norm=[sqrt(1.-(sin(obsza*!DtoR)*sin((obsaz)*!DtoR))^2.),sqrt(1.-(sin(obsza*!DtoR)*cos((obsaz)*!DtoR))^2.)]
;NOTE: Eq2Hor REQUIRES Jdate to have the same number of elements as RA and Dec for precession!!
;;NOTE: The NEW Eq2Hor REQUIRES Jdate to be a scalar! They created a new bug when they fixed the old one
Eq2Hor,ra_use,dec_use,Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
za_arr=fltarr(psf_dim2,psf_dim2)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_dim2,psf_dim2) & az_arr[valid_i]=az_arr1

xvals3=za_arr*Sin(az_arr*!DtoR)
yvals3=za_arr*Cos(az_arr*!DtoR)

hour_angle=obs.obsra - ra_use
h_neg = where(hour_angle LT 0, N_neg)
IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
hour_angle = hour_angle mod 360.
hadec2altaz, hour_angle, dec_use, obs.obsdec, elevation_use, azimuth_use
elevation_arr=fltarr(psf_dim2,psf_dim2) & elevation_arr[valid_i]=elevation_use
azimuth_arr=fltarr(psf_dim2,psf_dim2) & azimuth_arr[valid_i]=azimuth_use

gain_tile_i=reform(gain_array_X[0,*])
gain_freq_bin_i=findgen(N_Elements(gain_tile_i)) mod nfreq_bin
IF Keyword_Set(swap_pol) THEN pol_arr=[[1,1],[0,0],[1,0],[0,1]] ELSE pol_arr=[[0,0],[1,1],[0,1],[1,0]] 

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
    gain1_full=(pol1 EQ 0) ? gain_array_X:gain_array_Y
    gain2_full=(pol2 EQ 0) ? gain_array_X:gain_array_Y
    
    freq_norm_check=fltarr(nfreq_bin)+1.
    
    FOR freq_i=0,nfreq_bin-1 DO BEGIN        
        t2_a=Systime(1)
        antenna_beam_arr1=Ptrarr(16,/allocate)
        antenna_beam_arr2=Ptrarr(16,/allocate)
        beam1_arr=Ptrarr(n_tiles,/allocate)
        beam2_arr=Ptrarr(n_tiles,/allocate)
        
        gain1=gain1_full[1:*,where(gain_freq_bin_i EQ freq_i)]
        gain2=gain2_full[1:*,where(gain_freq_bin_i EQ freq_i)]
        gain1_avg=Median(gain1,dimension=2)
        gain2_avg=Median(gain2,dimension=2)
        
        ;mwa_tile_beam_generate.pro paper_tile_beam_generate.pro
        beam1_0=Call_function(tile_beam_fn,gain1_avg,antenna_beam_arr1,$ ;mwa_tile_beam_generate
            frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3,$
            ra_arr=ra_arr2,dec_arr=dec_arr2,delay_settings=delay_settings,dimension=psf_dim2)
        IF pol2 EQ pol1 THEN antenna_beam_arr2=antenna_beam_arr1
        beam2_0=Call_function(tile_beam_fn,gain2_avg,antenna_beam_arr2,$
            frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3,$
            ra_arr=ra_arr2,dec_arr=dec_arr2,delay_settings=delay_settings,dimension=psf_dim2)
        Ptr_free,antenna_beam_arr1,antenna_beam_arr2
        t3_a=Systime(1)
        t2+=t3_a-t2_a
        psf_base1=dirty_image_generate(beam1_0*Conj(beam2_0),/no_real)
        
        uv_mask=fltarr(psf_dim2,psf_dim2)
        beam_i=region_grow(abs(psf_base1),psf_dim2*(1.+psf_dim2)/2.,thresh=[Max(abs(psf_base1))/beam_mask_threshold,Max(abs(psf_base1))])
        uv_mask[beam_i]=1.
        
        psf_base2=psf_base1*psf_resolution^2.
        uv_mask2=uv_mask
;        psf_base2=Interpolate(psf_base1,psf_xvals1,psf_yvals1,cubic=-0.5)
;        uv_mask2=Interpolate(uv_mask,psf_xvals1,psf_yvals1)
        phase_test=Atan(psf_base2,/phase)*!Radeg
        phase_cut=where(Abs(phase_test) GE 90.,n_phase_cut)
        IF n_phase_cut GT 0 THEN uv_mask2[phase_cut]=0
        psf_base2*=uv_mask2
        freq_norm_check[freq_i]=Total(Abs(psf_base2))/psf_resolution^2.
        gain_normalization=1./(Total(Abs(psf_base2))/psf_resolution^2.)
;        psf_base2*=gain_normalization
;        psf_base2*=freq_norm[freq_i]
;        psf_base2*=pol_norm[pol_i]
        t4_a=Systime(1)
        t3+=t4_a-t3_a
        phase_mag=(Abs(Atan(psf_base2,/phase))<Abs(!Pi-Abs(Atan(psf_base2,/phase))))*Floor(uv_mask2>0)
        IF Max(phase_mag) GT !Pi*residual_tolerance THEN complex_flag_arr[pol_i,freq_i]=1
        
;        FOR tile_i=0,n_tiles-1 DO BEGIN
;            *beam1_arr[tile_i]=Call_function(tile_beam_fn,gain1[*,tile_i],antenna_beam_arr1,$
;                frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3,$
;                ra_arr=ra_arr2,dec_arr=dec_arr2,delay_settings=delay_settings,dimension=dimension)
;            *beam2_arr[tile_i]=Call_function(tile_beam_fn,gain2[*,tile_i],antenna_beam_arr2,$
;                frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,obsaz=obsaz,obsza=obsza,$
;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3,$
;                ra_arr=ra_arr2,dec_arr=dec_arr2,delay_settings=delay_settings,dimension=dimension)
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
;            residual_single=psf_single-psf_base2
;            i_res=where(residual_single GE ((psf_base2*residual_tolerance)>residual_threshold),nres)
;            psf_residuals_n[pol_i,freq_i,bi]=nres
;            IF nres GT 0 THEN BEGIN
;                psf_residuals_i[pol_i,freq_i,bi]=Ptr_new(i_res)
;                psf_residuals_val[pol_i,freq_i,bi]=Ptr_new(residual_single[i_res])
;            ENDIF
;        ENDFOR
        Ptr_free,antenna_beam_arr1,antenna_beam_arr2,beam1_arr,beam2_arr
        FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
            psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j]=$
                Ptr_new(psf_base2[xvals_i+i,yvals_i+j]) 
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
    n_pol=n_pol,n_freq=n_freq,freq_cen=freq_center)
IF ~Keyword_Set(no_save) THEN save,psf,filename=file_path_fhd+'_beams'+'.sav',/compress
t5=Systime(1)-t5_a
timing=Systime(1)-t00
IF ~Keyword_Set(silent) THEN print,[timing,t1,t2,t3,t4,t5]
RETURN,psf
END
