FUNCTION vis_source_model,skymodel, obs, status_str, psf, params, vis_weight_ptr, cal, jones, model_uv_arr=model_uv_arr,$
    file_path_fhd=file_path_fhd, timing=timing, silent=silent, uv_mask=uv_mask, error=error, beam_arr=beam_arr,$
    fill_model_visibilities=fill_model_visibilities, use_pointing_center=use_pointing_center, vis_model_ptr=vis_model_ptr,$
    spectral_model_uv_arr=spectral_model_uv_arr,model_delay_filter=model_delay_filter, model_transfer=model_transfer,$
    model_uv_transfer=model_uv_transfer,_Extra=extra

;; Option to transfer pre-made and unflagged model visbilities
if keyword_set(model_transfer) then begin
   vis_arr = vis_model_transfer(obs,model_transfer)
   return, vis_arr
endif

fill_model_visibilities=1
t0=Systime(1)
IF N_Elements(error) EQ 0 THEN error=0
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
IF N_Elements(silent) EQ 0 THEN silent=1

IF N_Elements(skymodel) EQ 0 THEN fhd_save_io,status_str,skymodel,var='skymodel',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF Min(Ptr_valid(vis_weight_ptr)) EQ 0 THEN fhd_save_io,status_str,vis_weight_ptr,var='vis_weights',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(jones) EQ 0 THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra

IF Keyword_Set(skymodel) THEN BEGIN
    galaxy_flag=skymodel.galaxy_model
    diffuse_filepath=skymodel.diffuse_model
    n_sources=skymodel.n_sources
ENDIF ELSE BEGIN
    galaxy_flag=0
    diffuse_filepath=''
    n_sources=0
ENDELSE
heap_gc

pol_names=obs.pol_names

;extract information from the structures
n_pol=obs.n_pol
n_spectral=obs.degrid_spectral_terms
dimension=Long(obs.dimension)
elements=Long(obs.elements)
degpix=obs.degpix
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
icomp=Complex(0,1)

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

IF Keyword_Set(uv_mask) THEN uv_mask_use=uv_mask ELSE uv_mask_use=Fltarr(dimension,elements)+1
uv_mask_use[*,elements/2+psf.dim:*]=0. 

freq_bin_i=(*obs.baseline_info).fbin_i
frequency_array=(*obs.baseline_info).freq
nfreq_bin=Max(freq_bin_i)+1
nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq=obs.n_freq

;Set up the structures such that a double bandwidth model is made
if keyword_set(model_delay_filter) then begin
  obs_original = replicate(obs,1)
  psf_original = replicate(psf,1)
  freq_bin_i=[INTARR(obs.n_freq/2)+freq_bin_i[0],freq_bin_i,INTARR(obs.n_freq/2)+freq_bin_i[obs.n_freq-1]]
  for i=1,obs.n_freq/2 do frequency_array = [(*obs.baseline_info).freq[0]-obs.freq_res*i,frequency_array]
  for i=1,obs.n_freq/2 do frequency_array = [frequency_array,(*obs.baseline_info).freq[N_elements((*obs.baseline_info).freq)-1]+obs.freq_res*i]
  
  nfreq_bin=Max(freq_bin_i)+1
  n_freq = n_freq*2
  freq_use = INTARR(n_freq) + 1
  
  updated_values = {n_freq:n_freq, freq_use:freq_use, freq:FLOAT(frequency_array), fbin_i:Long(freq_bin_i)}
  obs = structure_update(obs,_Extra=updated_values)
  obs.baseline_info = Pointer_copy(obs_original.baseline_info)
  *obs.baseline_info = structure_update(*obs.baseline_info,_Extra=updated_values)
    
  updated_values = {n_freq:nfreq_bin, freq_use:freq_use, freq:FLOAT(frequency_array), fbin_i:Long(freq_bin_i)}
  psf = structure_update(psf,_Extra=updated_values)
endif

n_freq_bin=N_Elements(freq_bin_i)
IF N_Elements(vis_model_ptr) LT n_pol THEN vis_model_ptr=intarr(n_pol)
IF n_spectral EQ 0 THEN spectral_model_uv_arr=intarr(n_pol)

vis_dimension=Float(nbaselines*n_samples)

IF Min(Ptr_valid(model_uv_arr)) EQ 0 THEN BEGIN
    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(dimension,elements)
ENDIF
IF (Min(Ptr_valid(spectral_model_uv_arr)) EQ 0) AND (n_spectral GT 0) THEN BEGIN
    spectral_model_uv_arr=Ptrarr(n_pol,n_spectral,/allocate)
    FOR pol_i=0,n_pol-1 DO FOR s_i=0,n_spectral-1 DO *spectral_model_uv_arr[pol_i,s_i]=Complexarr(dimension,elements)
ENDIF

if ~keyword_set(model_uv_transfer) then begin
    IF n_sources GT 0 THEN BEGIN ;test that there are actual sources in the source list
        ;convert Stokes entries to instrumental polarization (weighted by one factor of the beam) 
        ;NOTE this is for record-keeping purposes, since the Stokes flux values will actually be used
        source_list=skymodel.source_list
        source_list.extend=Pointer_copy(source_list.extend)
        source_list=stokes_cnv(source_list,jones,beam_arr=beam_arr,/inverse,_Extra=extra)
        model_uv_arr1=source_dft_model(obs,jones,source_list,t_model=t_model,sigma_threshold=2.,$
            spectral_model_uv_arr=spectral_model_uv_arr1,uv_mask=uv_mask_use,_Extra=extra)
        FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*model_uv_arr1[pol_i];*uv_mask_use 
        FOR pol_i=0,n_pol-1 DO FOR s_i=0,n_spectral-1 DO *spectral_model_uv_arr[pol_i,s_i]+=*spectral_model_uv_arr1[pol_i,s_i];*uv_mask_use
        undefine_fhd,model_uv_arr1,spectral_model_uv_arr1,source_list
        IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
    ENDIF
endif

IF galaxy_flag THEN gal_model_uv=fhd_galaxy_model(obs,jones,spectral_model_uv_arr=gal_spectral_model_uv,antialias=1,/uv_return,_Extra=extra)
IF Min(Ptr_valid(gal_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*gal_model_uv[pol_i];*uv_mask_use
IF Min(Ptr_valid(gal_spectral_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO FOR s_i=0,n_spectral-1 DO $
    *spectral_model_uv_arr[pol_i,s_i]+=*gal_spectral_model_uv[pol_i,s_i];*uv_mask_use
undefine_fhd,gal_model_uv,gal_spectral_model_uv

IF Keyword_Set(diffuse_filepath) THEN BEGIN
    IF file_test(diffuse_filepath) EQ 0 THEN diffuse_filepath=(file_search(diffuse_filepath+'*'))[0]
    print,"Reading diffuse model file: "+diffuse_filepath 
    diffuse_model_uv=fhd_diffuse_model(obs,jones,skymodel,spectral_model_arr=diffuse_spectral_model_uv,/uv_return,model_filepath=diffuse_filepath,_Extra=extra)
    IF Max(Ptr_valid(diffuse_model_uv)) EQ 0 THEN print,"Error reading or building diffuse model. Null pointer returned!"
ENDIF
IF Min(Ptr_valid(diffuse_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*diffuse_model_uv[pol_i];*uv_mask_use
IF Min(Ptr_valid(diffuse_spectral_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO FOR s_i=0,n_spectral-1 DO $
    *spectral_model_uv_arr[pol_i,s_i]+=*diffuse_spectral_model_uv[pol_i,s_i];*uv_mask_use
undefine_fhd,diffuse_model_uv,diffuse_spectral_model_uv

fhd_save_io,status_str,model_uv_arr,var='model_uv_arr',file_path_fhd=file_path_fhd,_Extra=extra
vis_arr=Ptrarr(n_pol)

valid_test=fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO valid_test[pol_i]=Total(Abs(*model_uv_arr[pol_i])) ; if the model only contains unpolarized sources but n_pol is set > 2, then this test will fail. Set n_pol=2.
IF min(valid_test) EQ 0 THEN BEGIN
    error=1
    print,"ERROR: Invalid model."
    timing=Systime(1)-t0
    RETURN,vis_arr
ENDIF

t_degrid=Fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    vis_arr[pol_i]=visibility_degrid(*model_uv_arr[pol_i],vis_weight_ptr[pol_i],obs,psf,params,silent=silent,$
        timing=t_degrid0,polarization=pol_i,fill_model_visibilities=fill_model_visibilities,$
        vis_input_ptr=vis_model_ptr[pol_i],spectral_model_uv_arr=spectral_model_uv_arr[pol_i,*], _Extra=extra)
    t_degrid[pol_i]=t_degrid0
ENDFOR
IF ~Keyword_Set(silent) THEN print,"Degridding timing: ",strn(t_degrid)

if keyword_set(model_delay_filter) then begin  
  vis_delay_filter, vis_arr, params, obs
  psf = replicate(psf_original,1)
  obs = replicate(obs_original,1)
endif
timing=Systime(1)-t0

RETURN,vis_arr
END
