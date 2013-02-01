;+
; :Description:
;    Generates the gridded beam models for each baseline/frequency to be used for gridding visibilities.
;
; :Params:
;    obs - structure containing details of the observation
;
; :Keywords:
;    data_directory - working directory
;    
;    filename - uvfits filename, omitting the .uvfits extension. If the data is already calibrated, it should end with .cal.uvfits instead of just .uvfits
;    
;    restore_last - set to restore filename_beams(file_id).sav instead of recalculating
;    
;    residual_tolerance
;    
;    residual_threshold
;    
;
; :Author: isullivan
;-
FUNCTION beam_setup,obs,file_path_fhd,restore_last=restore_last,$
    residual_tolerance=residual_tolerance,residual_threshold=residual_threshold,$
    instrument=instrument,silent=silent,psf_dim=psf_dim,psf_resolution=psf_resolution,_Extra=extra

compile_opt idl2,strictarrsubs  

;vis_path_default,data_directory,filename,file_path,obs=obs,version=version
IF Keyword_Set(restore_last) AND (file_test(file_path_fhd+'_beams'+'.sav') EQ 0) THEN BEGIN 
    print,file_path_fhd+'_beams'+'.sav' +' Not found. Recalculating.' 
    restore_last=0
ENDIF
IF Keyword_Set(restore_last) THEN BEGIN
    print,'Saved beam model restored'
    restore,file_path_fhd+'_beams'+'.sav'
    RETURN,psf
ENDIF

IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)
tile_beam_fn=instrument+'_tile_beam_generate' ;mwa_tile_beam_generate
IF instrument EQ 'paper' THEN base_gain=fltarr(1)+1.
;Fixed parameters 
IF N_Elements(obs) EQ 0 THEN restore,file_path+'_obs.sav'
dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
degpix=obs.degpix
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=32. ;=32?
IF N_Elements(psf_dim) EQ 0 THEN psf_dim=Ceil(2.*!Pi/kbinsize) ;=16?
psf_dim2=psf_dim*psf_resolution

degpix_use=!RaDeg/(kbinsize*psf_dim)

;residual_tolerance is residual as fraction of psf_base above which to include 
IF N_Elements(residual_tolerance) EQ 0 THEN residual_tolerance=1./100.  
;residual_threshold is minimum residual above which to include
IF N_Elements(residual_threshold) EQ 0 THEN residual_threshold=0.

;extract information from the structures
n_tiles=obs.n_tile
n_frequencies=obs.n_freq
n_pol=obs.n_pol

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B

frequency_array=obs.freq
freq_bin_i=obs.fbin_i
nfreq_bin=Max(freq_bin_i)+1

freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO freq_center[fi]=Median(frequency_array[where(freq_bin_i EQ fi)])
bin_offset=(*obs.baseline_info).bin_offset
nbaselines=bin_offset[1]

obsra=obs.obsra
obsdec=obs.obsdec
zenra=obs.zenra
zendec=obs.zendec
Jdate=obs.Jd0
;rotation=obs.rotation

beam_setup_init,gain_array_X,gain_array_Y,file_path_fhd,n_tiles=n_tiles,nfreq_bin=nfreq_bin,base_gain=base_gain

;begin forming psf
psf_residuals_i=Ptrarr(n_pol,nfreq_bin,nbaselines,/allocate) ;contains arrays of pixel indices of pixels with modified psf for a given baseline id
psf_residuals_val=Ptrarr(n_pol,nfreq_bin,nbaselines,/allocate) ;contains arrays of values corresponding to the pixel indices above
psf_residuals_n=fltarr(n_pol,nfreq_bin,nbaselines) ;contains the total number of modified pixels for each baseline id

psf_base=Ptrarr(n_pol,nfreq_bin,psf_resolution,psf_resolution,/allocate)
psf_xvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
psf_yvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
xvals_i=meshgrid(psf_dim,psf_dim,1)*psf_resolution
yvals_i=meshgrid(psf_dim,psf_dim,2)*psf_resolution
xvals=meshgrid(psf_dim2,psf_dim2,1)/psf_resolution-psf_dim/2.
yvals=meshgrid(psf_dim2,psf_dim2,2)/psf_resolution-psf_dim/2.

;vis_coordinates,degpix=degpix_use,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
;    dimension=psf_dim2,rotation=rotation,valid_i=valid_i,astr=astr,zen_astr=zen_astr

projection_slant_orthographic,astr=astr,valid_i=valid_i,$
    degpix=degpix_use,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=psf_dim2
Eq2Hor,obsra,obsdec,Jdate,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
intensity0=stokes_off_zenith(obsaz, obsalt, [1.,0.,0.,0.], Ex0, Ey0,/intensity)
norm=Sqrt(2.)*[ex0,ey0]

xvals2=meshgrid(psf_dim2,psf_dim2,1)
yvals2=meshgrid(psf_dim2,psf_dim2,2)

;xy2ad,xvals2,yvals2,zen_astr,ra_arr_use0,dec_arr_use0  
;valid_i0=where(Finite(ra_arr_use0))
;;vis_coordinates,0,ra_arr_use0,dec_arr_use0,degpix=degpix_use,obsra=zenra,obsdec=zendec,zenra=zenra,zendec=zendec,$
;;    dimension=psf_dim2,rotation=0,valid_i=valid_i0
;Eq2Hor,ra_arr_use0[valid_i0],dec_arr_use0[valid_i0],Jdate,alt_arr0,az_arr0a,lat=obs.lat,lon=obs.lon,alt=obs.alt
;za_arr0=fltarr(psf_dim2,psf_dim2)+90. & za_arr0[valid_i0]=90.-alt_arr0
;az_arr0=fltarr(psf_dim2,psf_dim2) & az_arr0[valid_i0]=az_arr0a
;
;intensity=stokes_off_zenith(az_arr0a, alt_arr0, [1.,0.,0.,0.], Ex_mag0, Ey_mag0,/intensity)
;proj0=Ptrarr(2,/allocate)
;proj0_x=fltarr(psf_dim2,psf_dim2) & proj0_x[valid_i0]=Ey_mag0
;proj0_y=fltarr(psf_dim2,psf_dim2) & proj0_y[valid_i0]=Ex_mag0
;;*proj0[0]=proj0_x^2.
;;*proj0[1]=proj0_y^2.
;*proj0[0]=proj0_x
;*proj0[1]=proj0_y

xy2ad,xvals2,yvals2,astr,ra_arr_use1,dec_arr_use1  
valid_i=where(Finite(ra_arr_use1))
Eq2Hor,ra_arr_use1[valid_i],dec_arr_use1[valid_i],Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt
za_arr=fltarr(psf_dim2,psf_dim2)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_dim2,psf_dim2) & az_arr[valid_i]=az_arr1

IF Abs(obs.obsra-obs.zenra) GT 90. THEN lon_offset=obs.obsra-((obs.obsra GT obs.zenra) ? 360.:(-360.))-obs.zenra ELSE lon_offset=obs.obsra-obs.zenra
lat_offset=-(obs.zendec-obs.obsdec)
degpix_use3=[Cos(lon_offset*!DtoR*Cos(obs.obsdec*!DtoR)),Cos(lat_offset*!DtoR)]*degpix_use
xvals3=za_arr*Sin(az_arr*!DtoR);/degpix_use3[0]
yvals3=za_arr*Cos(az_arr*!DtoR);/degpix_use3[1]

el_arr=90.-za_arr
polarization_map=polarization_map_create(az_arr, el_arr,stokes_zenith=[1.,0,0,0])
proj=[polarization_map[0,0],polarization_map[0,1],polarization_map[2,2],polarization_map[2,3]]

;ad2xy,obsra,obsdec,astr,obsx,obsy
;ad2xy,zenra,zendec,astr,zenx,zeny
;dxc=obsx-zenx
;dyc=obsy-zeny
;IF Abs(obs.obsra-obs.zenra) GT 90. THEN lon_offset=obs.obsra-((obs.obsra GT obs.zenra) ? 360.:(-360.))-obs.zenra ELSE lon_offset=obs.obsra-obs.zenra
;lat_offset=-(obs.zendec-obs.obsdec)
;degpix_use2=[Cos(lon_offset*!DtoR*Cos(obs.obsdec*!DtoR)),Cos(lat_offset*!DtoR)]*degpix_use
;xcvals2=((xvals2+dxc-psf_dim2/2.)*Cos(!DtoR*obs.rotation)*degpix_use2[0]+(yvals2+dyc-psf_dim2/2.)*degpix_use2[1]*Sin(!DtoR*obs.rotation));*degpix_use2[0]
;ycvals2=((yvals2+dyc-psf_dim2/2.)*degpix_use2[1]*Cos(!DtoR*obs.rotation)-(xvals2+dxc-psf_dim2/2.)*degpix_use2[0]*Sin(!DtoR*obs.rotation));*degpix_use2[1]
;;proj_x=Exp(Complex(0,1)*xcvals2*!DtoR/2.)/Sqrt(2.)
;;proj_y=Exp(Complex(0,1)*ycvals2*!DtoR/2.)/Sqrt(2.)
;proj_x=Cos(xcvals2*!DtoR)/Sqrt(2.)
;proj_y=Cos(ycvals2*!DtoR)/Sqrt(2.)
;xi_use=region_grow(proj_x,psf_dim2*(1.+psf_dim2)/2.,thresh=[0,1])
;mask_x=fltarr(psf_dim2,psf_dim2) & mask_x[xi_use]=1. 
;yi_use=region_grow(proj_y,psf_dim2*(1.+psf_dim2)/2.,thresh=[0,1])
;mask_y=fltarr(psf_dim2,psf_dim2) & mask_y[yi_use]=1. 
;proj=Ptrarr(2,/allocate)
;*proj[0]=proj_x;*mask_x
;*proj[1]=proj_y;*mask_y
;
;za_arr=angle_difference(0,0,xcvals2,ycvals2,/degree)
;az_arr=Atan(-xcvals2,-ycvals2)*!Radeg+180.

;xcvals2a=((xvals2-psf_dim2/2.)*Cos(!DtoR*obs.rotation)*degpix_use2[0]+(yvals2-psf_dim2/2.)*degpix_use2[1]*Sin(!DtoR*obs.rotation));*degpix_use2[0]
;ycvals2a=((yvals2-psf_dim2/2.)*degpix_use2[1]*Cos(!DtoR*obs.rotation)-(xvals2-psf_dim2/2.)*degpix_use2[0]*Sin(!DtoR*obs.rotation));*degpix_use2[1]
;;proj_x0=Exp(Complex(0,1)*xcvals2a*!DtoR/2.)/Sqrt(2.)
;;proj_y0=Exp(Complex(0,1)*ycvals2a*!DtoR/2.)/Sqrt(2.)
;;proj_y=Exp(Complex(0,1)*ycvals2*!DtoR/2.)/Sqrt(2.)
;proj_x0=Cos(xcvals2a*!DtoR)/Sqrt(2.)
;proj_y0=Cos(ycvals2a*!DtoR)/Sqrt(2.)
;proj0=Ptrarr(2,/allocate)
;*proj0[0]=proj_x0
;*proj0[1]=proj_y0

;;TEMPORARY ADDITION
;;*proj0[0]=fltarr(psf_dim2,psf_dim2)+sqrt(2)
;;*proj0[1]=fltarr(psf_dim2,psf_dim2)+sqrt(2)
;*proj[0]=fltarr(psf_dim2,psf_dim2)+sqrt(2)
;*proj[1]=fltarr(psf_dim2,psf_dim2)+sqrt(2)


FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN 
    *psf_xvals[i,j]=xvals[xvals_i+i,yvals_i+j]
    *psf_yvals[i,j]=yvals[xvals_i+i,yvals_i+j]
ENDFOR

;polarization ids are 0:XX, 1:YY, 2:XY, 3:YX

gain_tile_i=reform(gain_array_X[0,*])
gain_freq_bin_i=findgen(N_Elements(gain_tile_i)) mod nfreq_bin
pol_arr=[[0,0],[1,1],[0,1],[1,0]] 
t1=Systime(1)

IF not Keyword_Set(silent) THEN print,'Building beam model. Time elapsed: estimated time remaining'
FOR pol_i=0,n_pol-1 DO BEGIN

    pol1=pol_arr[0,pol_i]
    pol2=pol_arr[1,pol_i]
    gain1_full=(pol1 EQ 0) ? gain_array_X:gain_array_Y
    gain2_full=(pol2 EQ 0) ? gain_array_X:gain_array_Y
    
    FOR freq_i=0,nfreq_bin-1 DO BEGIN        
        antenna_beam_arr1=Ptrarr(16,/allocate)
        antenna_beam_arr2=Ptrarr(16,/allocate)
        beam1_arr=Ptrarr(n_tiles,/allocate)
        beam2_arr=Ptrarr(n_tiles,/allocate)
        
        gain1=gain1_full[1:*,where(gain_freq_bin_i EQ freq_i)]
        gain2=gain2_full[1:*,where(gain_freq_bin_i EQ freq_i)]
        gain1_avg=Median(gain1,dimension=2)
        gain2_avg=Median(gain2,dimension=2)
        
        ;mwa_tile_beam_generate.pro
        beam1_0=Call_function(tile_beam_fn,gain1_avg,antenna_beam_arr1,$ ;mwa_tile_beam_generate
            frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3)
        IF pol2 EQ pol1 THEN antenna_beam_arr2=antenna_beam_arr1
        beam2_0=Call_function(tile_beam_fn,gain2_avg,antenna_beam_arr2,$
            frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,$
            psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,xvals=xvals3,yvals=yvals3)
        
;;        psf_base0=dirty_image_generate((beam1_0*(*proj0[pol1]))*Conj(beam2_0*(*proj0[pol2])),/no_real)
;        psf_base1=dirty_image_generate((beam1_0*(*proj[pol1]))*Conj(beam2_0*(*proj[pol2])),/no_real)
        psf_base1=dirty_image_generate(beam1_0*Conj(beam2_0)*(*proj[pol_i]),/no_real)
;        psf_base1=dirty_image_generate(beam1_0*Conj(beam2_0),/no_real)
        
;        psf_base1=abs(psf_base1)
        
        uv_mask=fltarr(psf_dim2,psf_dim2)
        beam_i=region_grow(abs(psf_base1),psf_dim2*(1.+psf_dim2)/2.,thresh=[Max(abs(psf_base1))/1e3,Max(abs(psf_base1))])
        uv_mask[beam_i]=1.
;        psf_base0*=uv_mask
        psf_base1*=uv_mask
;       
;        gain_normalization=norm[pol1]*norm[pol2]/(Total(psf_base1)/psf_resolution^2.)
        gain_normalization=norm[pol1]*norm[pol2]/(Total(Abs(psf_base1))/psf_resolution^2.)
        psf_base1*=gain_normalization
;        gain_normalization=1./(Total(psf_base0)/psf_resolution^2.)
;        psf_base1*=gain_normalization
        
        FOR tile_i=0,n_tiles-1 DO BEGIN
            *beam1_arr[tile_i]=Call_function(tile_beam_fn,gain1[*,tile_i],antenna_beam_arr1,$
                frequency=freq_center[freq_i],polarization=pol1,za_arr=za_arr,az_arr=az_arr,$
                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize)
            *beam2_arr[tile_i]=Call_function(tile_beam_fn,gain2[*,tile_i],antenna_beam_arr2,$
                frequency=freq_center[freq_i],polarization=pol2,za_arr=za_arr,az_arr=az_arr,$
                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize)
        ENDFOR
        
        FOR bi=0,nbaselines-1 DO BEGIN
            IF Min((gain1[*,tile_A[bi]-1]-gain1_avg EQ fltarr(16)) AND (gain2[*,tile_B[bi]-1]-gain2_avg EQ fltarr(16))) THEN BEGIN
                psf_residuals_n[pol_i,freq_i,bi]=0
                CONTINUE
            ENDIF
;;            beam1=Call_function(tile_beam_fn,gain1[*,tile_A[bi]],antenna_beam_arr1,$
;;                frequency=freq_center[freq_i],angle_offset=ang_off,polarization=pol1,$
;;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
;;                foreshorten_U=fshort_U,foreshorten_V=fshort_V)
;;            beam2=Call_function(tile_beam_fn,gain2[*,tile_B[bi]],antenna_beam_arr2,$
;;                frequency=freq_center[freq_i],angle_offset=ang_off,polarization=pol2,$
;;                psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
;;                foreshorten_U=fshort_U,foreshorten_V=fshort_V)
;            beam1=*beam1_arr[tile_A[bi]-1]
;            beam2=*beam2_arr[tile_B[bi]-1]
;            psf_single=Convolve2(*beam1_arr[tile_A[bi]-1],*beam2_arr[tile_B[bi]-1],pad=convolve_pad,/absolute)*gain_normalization
            psf_single=dirty_image_generate(*beam1_arr[tile_A[bi]-1],*beam2_arr[tile_B[bi]-1])*uv_mask*gain_normalization
            residual_single=psf_single-psf_base1
            i_res=where(residual_single GE ((psf_base1*residual_tolerance)>residual_threshold),nres)
            psf_residuals_n[pol_i,freq_i,bi]=nres
            IF nres GT 0 THEN BEGIN
                *psf_residuals_i[pol_i,freq_i,bi]=i_res
                *psf_residuals_val[pol_i,freq_i,bi]=residual_single[i_res]
            ENDIF
        ENDFOR
        Ptr_free,antenna_beam_arr1,antenna_beam_arr2,beam1_arr,beam2_arr
        FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
            *psf_base[pol_i,freq_i,i,j]=psf_base1[xvals_i+i,yvals_i+j];/Total(psf_base1[xvals_i+i,yvals_i+j]) 
        breakpoint0=0
    ENDFOR
ENDFOR

psf=vis_struct_init_psf(base=psf_base,res_i=psf_residuals_i,res_val=psf_residuals_val,$
    res_n=psf_residuals_n,xvals=psf_xvals,yvals=psf_yvals,norm=norm)
save,psf,filename=file_path_fhd+'_beams'+'.sav',/compress
RETURN,psf
END
