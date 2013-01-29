
;+
; :Description:
;    uvfits2fhd is the main program for working with uvfits data. 
;    It will read the uvfits file, grid the data, generate the holographic mapping functions, 
;    and run Fast Holographic Deconvolution
;
;
;
; :Keywords:
;    data_directory - working directory
;    
;    filename - uvfits filename, omitting the .uvfits extension. If the data is already calibrated, it should end with _cal.uvfits instead of just .uvfits
;    
;    beam_recalculate - if set, generates a new beam model
;    
;    mapfn_recalculate - if not set to 0, will generate Holographic Mapping Functions for each polarization
;    
;    dimension - desired dimension in pixels of the final images
;    
;    kbinsize - pixel size in wavelengths of the uv image. 
;    
;    n_pol - 1: use xx only, 2: use xx and xy, 4: use xx, yy, xy, and yx (Default: as many as are available)
;    
;    flag - set to look for anomalous visibility data and update flags (default=1, also set to 1 if '_flags.sav' does not exist)
;    
;    Extra - pass any non-default parameters to fast_holographic_deconvolution through this parameter 
;
; :Author: isullivan 2012
;-
PRO uvfits2fhd,file_path_vis,export_images=export_images,$
    beam_recalculate=beam_recalculate,mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,flag=flag,silent=silent,GPU_enable=GPU_enable,deconvolve=deconvolve,$
    rephase_to_zenith=rephase_to_zenith,CASA_calibration=CASA_calibration,healpix_recalculate=healpix_recalculate,$
    file_path_fhd=file_path_fhd,_Extra=extra

compile_opt idl2,strictarrsubs    
except=!except
!except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
heap_gc 
t0=Systime(1)
;IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(calibrate) EQ 0 THEN calibrate=0
;IF N_Elements(cut_baselines) EQ 0 THEN cut_baselines=12;0 ;minimum baseline threshold to cut BEFORE processing. If negative, specifies a maximum instead.
IF N_Elements(min_baseline) EQ 0 THEN min_baseline=12.
IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=1
IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=1
IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=1
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=1
IF N_Elements(flag) EQ 0 THEN flag=1.
IF N_Elements(deconvolve) EQ 0 THEN deconvolve=1
IF N_Elements(CASA_calibration) EQ 0 THEN CASA_calibration=1
;IF N_Elements(GPU_enable) EQ 0 THEN GPU_enable=0
;IF Keyword_Set(GPU_enable) THEN BEGIN
;    Defsysv,'GPU',exist=gpuvar_exist
;    IF gpuvar_exist eq 0 THEN GPUinit
;    IF !GPU.mode NE 1 THEN GPU_enable=0
;ENDIF

;file_path_fhd=fhd_path_setup(file_path_vis,data_directory=data_directory,filename=filename,version=version,$
;    output_dir=output_dir,output_filename=output_filename)

;vis_path_default,data_directory,filename,file_path,version=version,_Extra=extra
print,'Deconvolving: ',file_path_vis
print,'Output file_path:',file_path_fhd
ext='.uvfits'
header_filepath=file_path_fhd+'_header.sav'
flags_filepath=file_path_fhd+'_flags.sav'
;vis_filepath=file_path_fhd+'_vis.sav'
obs_filepath=file_path_fhd+'_obs.sav'
params_filepath=file_path_fhd+'_params.sav'
hdr_filepath=file_path_fhd+'_hdr.sav'

pol_names=['xx','yy','xy','yx','I','Q','U','V']

;info_struct=mrdfits(filepath(filename+ext,root_dir=rootdir('mwa'),subdir=data_directory),2,info_header,/silent)
data_struct=mrdfits(file_path_vis,0,data_header0,/silent)
;; testing for export type. If multibeam, then read original header
casa_type = strlowcase(strtrim(sxpar(data_header0, 'OBJECT'), 2))
if casa_type ne 'multi' then use_calheader = 1 else use_calheader = 0
if use_calheader eq 0 then begin
  IF file_test(header_filepath) EQ 0 THEN uvfits_header_casafix,file_path_vis,file_path_fhd=file_path_fhd
  RESTORE,header_filepath
  hdr=vis_header_extract(data_header,header2=data_header0, params = data_struct.params)
endif else begin
  hdr=vis_header_extract(data_header0, params = data_struct.params)

endelse

params=vis_param_extract(data_struct.params,hdr)
obs=vis_struct_init_obs(hdr,params,n_pol=n_pol,_Extra=extra)

IF Keyword_Set(rephase_to_zenith) THEN BEGIN
    print,"REPHASING VISIBILITIES TO POINT AT ZENITH!!"
    dimension=obs.dimension
    elements=obs.elements
    rotation=obs.rotation
    frequency_array=obs.freq
    kbinsize=obs.kpix
    kx_arr=params.uu/kbinsize
    ky_arr=params.vv/kbinsize
    xcen=frequency_array#kx_arr
    ycen=frequency_array#ky_arr
    phase_shift=Exp(Complex(0,1)*(2.*!Pi/dimension)*((obs.obsx-obs.zenx)*xcen+(obs.obsy-obs.zeny)*ycen))
    
    hdr.obsra=obs.zenra
    hdr.obsdec=obs.zendec
    obs=vis_struct_init_obs(hdr,params,n_pol=n_pol,rotation=rotation,_Extra=extra)
ENDIF ELSE phase_shift=1.

pol_dim=hdr.pol_dim
freq_dim=hdr.freq_dim
real_index=hdr.real_index
imaginary_index=hdr.imaginary_index
flag_index=hdr.flag_index
n_pol=obs.n_pol

data_array=data_struct.array
data_struct=0. ;free memory

flag_arr0=Reform(data_array[flag_index,*,*,*])
IF Keyword_Set(flag) THEN BEGIN
    print,'Flagging anomalous data'
    vis_flag,data_array,flag_arr0,obs,params,_Extra=extra
    SAVE,flag_arr0,filename=flags_filepath,/compress
ENDIF ELSE $ ;saved flags are needed for some later routines, so save them even if no additional flagging is done
    IF file_test(flags_filepath) NE 0 THEN RESTORE,flags_filepath ELSE SAVE,flag_arr0,filename=flags_filepath,/compress

save,obs,filename=obs_filepath
save,params,filename=params_filepath
save,hdr,filename=hdr_filepath
    
;Read in or construct a new beam model. Also sets up the structure PSF
print,'Calculating beam model'
psf=beam_setup(obs,file_path_fhd,restore_last=(Keyword_Set(beam_recalculate) ? 0:1),silent=silent,_Extra=extra)

beam=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO *beam[pol_i]=beam_image(psf,pol_i=pol_i,dimension=obs.dimension)


beam_mask=fltarr(obs.dimension,obs.elements)+1
FOR pol_i=0,(n_pol<2)-1 DO BEGIN
    mask0=fltarr(obs.dimension,obs.elements)
    mask_i=region_grow(*beam[pol_i],obs.obsx+obs.dimension*obs.obsy,thresh=[0.05,max(*beam[pol_i])])
    mask0[mask_i]=1
    beam_mask*=mask0
ENDFOR

IF Keyword_Set(healpix_recalculate) THEN hpx_cnv=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,$
    nside=nside,mask=beam_mask,radius=radius,restore_last=0,_Extra=extra)
hpx_cnv=0

vis_arr=Ptrarr(n_pol,/allocate)
flag_arr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    *vis_arr[pol_i]=Complex(reform(data_array[real_index,pol_i,*,*]),Reform(data_array[imaginary_index,pol_i,*,*]))*phase_shift
    *flag_arr[pol_i]=reform(flag_arr0[pol_i,*,*])
ENDFOR
;free memory
data_array=0 
flag_arr0=0

;IF Keyword_Set(calibrate) THEN FOR pol_i=0,n_pol-1 DO $
;    visibility_calibrate_simple,*vis_arr[pol_i],*flag_arr[pol_i],obs,params,beam=*beam[pol_i]

;save,vis_arr,filename=vis_filepath

t_grid=fltarr(n_pol)
t_mapfn_gen=fltarr(n_pol)

;Grid the visibilities
max_arr=fltarr(n_pol)
cal=fltarr(n_pol)
test_mapfn=1 & FOR pol_i=0,n_pol-1 DO test_mapfn*=file_test(file_path_fhd+'_uv_'+pol_names[0]+'.sav')
IF test_mapfn EQ 0 THEN grid_recalculate=1
test_mapfn=1 & FOR pol_i=0,n_pol-1 DO test_mapfn*=file_test(file_path_fhd+'_mapfn_'+pol_names[0]+'.sav')
IF test_mapfn EQ 0 THEN mapfn_recalculate=(grid_recalculate=1)
IF Keyword_Set(grid_recalculate) THEN BEGIN
    print,'Gridding visibilities'
    FOR pol_i=0,n_pol-1 DO BEGIN
;        IF Keyword_Set(GPU_enable) THEN $
;            dirty_UV=visibility_grid_GPU(*vis_arr[pol_i],*flag_arr[pol_i],obs,psf,params,timing=t_grid0,$
;                polarization=pol_i,weights=weights_grid,silent=silent,mapfn_recalculate=mapfn_recalculate) $
;        ELSE $
        dirty_UV=visibility_grid(*vis_arr[pol_i],*flag_arr[pol_i],obs,psf,params,file_path_fhd,timing=t_grid0,$
            polarization=pol_i,weights=weights_grid,silent=silent,mapfn_recalculate=mapfn_recalculate)
        t_grid[pol_i]=t_grid0
        dirty_img=dirty_image_generate(dirty_UV,baseline_threshold=0)
        
;        IF Keyword_Set(CASA_calibration) THEN BEGIN
;            norm=Max(*psf.base[pol_i,(Size(psf.base,/dimension))[1]/2.,0,0])
;            n_vis_orig=((obs.bin_offset)[1]-obs.n_tile)*obs.n_freq*Float(N_Elements(obs.bin_offset)) ;I have subtracted the auto-correlations
;;            norm*=n_vis_orig/obs.n_vis ;vis_flag updates obs.n_vis to only the number of unflagged visibilities (potentially need to update during gridding in case some are outside of the image!)
;            dirty_UV/=norm^2.
;            dirty_img/=norm^2.
;;            weights_grid*=norm
;        ENDIF
        
        save,dirty_UV,weights_grid,filename=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav'
        save,dirty_img,filename=file_path_fhd+'_dirty_'+pol_names[pol_i]+'.sav'
    ENDFOR
    print,'Gridding time:',t_grid
ENDIF ELSE BEGIN
    print,'Visibilities not re-gridded'
;    FOR pol_i=0,n_pol-1 DO BEGIN
;        restore,file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav'
;        restore,file_path_fhd+'_dirty_'+pol_names[pol_i]+'.sav'
;        norm0=Max(*psf.base[pol_i,(Size(psf.base,/dimension))[1]/2.,0,0])
;        n_vis_orig=((obs.bin_offset)[1]-obs.n_tile)*obs.n_freq*Float(N_Elements(obs.bin_offset))
;        
;        dirty_UV*=norm0^2.
;        dirty_img*=norm0^2.
;        save,dirty_UV,weights_grid,filename=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav'
;        save,dirty_img,filename=file_path_fhd+'_dirty_'+pol_names[pol_i]+'.sav'
;    ENDFOR
ENDELSE

IF Keyword_Set(export_images) THEN IF file_test(file_path_fhd+'_fhd.sav') EQ 0 THEN deconvolve=1

;deconvolve point sources using fast holographic deconvolution
IF Keyword_Set(deconvolve) THEN BEGIN
    print,'Deconvolving point sources'
    fhd_wrap,obs,params,psf,fhd,file_path_fhd=file_path_fhd,_Extra=extra,silent=silent,GPU_enable=GPU_enable
ENDIF ELSE print,'Gridded visibilities not deconvolved'

;Generate fits data files and images
IF Keyword_Set(export_images) THEN BEGIN
    print,'Exporting images'
;    ;Temporary addition:
;    fhd_paper_figures,restore_last=0,coord_debug=0,silent=0,show_grid=1,version=version,_Extra=extra
    
    fhd_output,obs,fhd,file_path_fhd=file_path_fhd,silent=silent,_Extra=extra
ENDIF

;;generate images showing the uv contributions of each tile. Very helpful for debugging!
;print,'Calculating individual tile uv coverage'
;mwa_tile_locate,obs=obs,params=params,psf=psf
timing=Systime(1)-t0
print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
!except=except
END