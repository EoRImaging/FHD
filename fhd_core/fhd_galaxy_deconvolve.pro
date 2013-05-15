FUNCTION fhd_galaxy_deconvolve,obs,image_uv_arr,map_fn_arr=map_fn_arr,beam_base=beam_base,model_uv_holo=model_uv_holo,$
    galaxy_model_img=galaxy_model_img,galaxy_model_uv=galaxy_model_uv,file_path_fhd=file_path_fhd,restore=restore,uv_return=uv_return

IF Keyword_Set(file_path_fhd) THEN file_path_galmodel=file_path_fhd+'_GalaxyModel.sav' ELSE file_path_galmodel=''
IF Keyword_Set(restore) AND file_test(file_path_galmodel) THEN BEGIN
    restore,file_path_galmodel
    IF Keyword_Set(uv_return) THEN RETURN,model_uv_holo ELSE RETURN,model_img_holo 
ENDIF

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr

freq_use=where((*obs.baseline_info).freq_use,nf_use)
f_bin=obs.fbin_i
fb_use=Uniq(f_bin[freq_use])
nbin=N_Elements(fb_use)
freq_arr=(obs.freq)[freq_use[fb_use]]/1E6
fb_hist=histogram(f_bin[freq_use],min=0,bin=1)
nf_arr=fb_hist[f_bin[freq_use[fb_use]]]

model_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr)

model=fltarr(dimension,elements)
FOR fi=0L,nbin-1 DO model+=*model_arr[fi]*nf_arr[fi]
model/=Total(nf_arr)
Ptr_free,model_arr

n_pol=N_Elements(image_uv_arr)
IF N_Elements(map_fn_arr) EQ 0 THEN BEGIN
    map_fn_arr=Ptrarr(n_pol,/allocate)
    file_path_mapfn=file_path_fhd+'_mapfn_'
    pol_names=['xx','yy','xy','yx','I','Q','U','V'] 
    restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;map_fn
    *map_fn_arr[pol_i]=Temporary(map_fn)
;    FOR pol_i=0,n_pol-1 DO *map_fn_arr[pol_i]=getvar_savefile(file_path_mapfn+pol_names[pol_i]+'.sav','map_fn')
ENDIF

model_uv=Ptrarr(n_pol)
model_uv_holo=Ptrarr(n_pol)
model_img_holo=Ptrarr(n_pol)
dirty_img=Ptrarr(n_pol)
scale_arr=fltarr(n_pol)

;weight=weight_invert(Cos((dec_arr-obs.zendec)*!DtoR)^2.)

FOR pol_i=0,n_pol-1 DO BEGIN
;    model_uv[pol_i]=Ptr_new(fft_shift(FFT(fft_shift(model*weight*(*beam_base[pol_i])),/inverse)))
;    model_uv[pol_i]=Ptr_new(fft_shift(FFT(fft_shift(model*hanning(dimension,elements)),/inverse)))
    model_uv[pol_i]=Ptr_new(fft_shift(FFT(fft_shift(model),/inverse)*(degpix*!DtoR)^2.))
    dirty_img[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix))
    model_uv_holo[pol_i]=Ptr_new(holo_mapfn_apply(*model_uv[pol_i],*map_fn_arr[pol_i],/indexed))
    model_img_holo[pol_i]=Ptr_new(dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix))
    beam_i=Region_grow(*beam_base[pol_i],Round(obs.obsx)+Round(obs.obsy)*dimension,threshold=[0.05,Max(*beam_base[pol_i])])
    beam_vals=(*beam_base[pol_i])[beam_i]
    model_vals=(*model_img_holo[pol_i])[beam_i]
    image_vals=(*dirty_img[pol_i])[beam_i]
    scale_arr[pol_i]=(linfit(model_vals,image_vals,measure_error=1./beam_vals))[1]
ENDFOR   
scale=Mean(scale_arr)
print,scale_arr
model*=scale

FOR pol_i=0,n_pol-1 DO BEGIN
    *model_uv[pol_i]*=scale_arr[pol_i]*dimension*elements
    *model_uv_holo[pol_i]*=scale_arr[pol_i]
    *model_img_holo[pol_i]*=scale_arr[pol_i]
ENDFOR
galaxy_model_img=model
galaxy_model_uv=model_uv

;comp_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr,/components)
;n_comp=N_Elements(comp_arr)
;
;model_uv=Ptrarr(n_comp)
;FOR ci=0,n_comp-1 DO model_uv[ci]=Ptr_new(fft_shift(FFT(fft_shift(*comp_arr[ci]),/inverse)))
;model_uv_holo=Ptrarr(n_comp,n_pol)
;model_img_holo=Ptrarr(n_comp,n_pol)
;dirty_img=Ptrarr(n_pol)
;scale_arr=fltarr(n_comp,n_pol)
;
;FOR pol_i=0,n_pol-1 DO BEGIN
;    dirty_img[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix))
;    FOR ci=0,n_comp-1 DO BEGIN
;        model_uv_holo[ci,pol_i]=Ptr_new(holo_mapfn_apply(model_uv[ci],*map_fn_arr[pol_i]))
;        model_img_holo[ci,pol_i]=Ptr_new(dirty_image_generate(*model_uv_holo[ci,pol_i],degpix=degpix))
;    ENDFOR
;    beam_i=Region_grow(*beam_base[pol_i],Round(obs.obsx)+Round(obs.obsy)*dimension,threshold=[0.2,Max(*beam_base[pol_i])])
;    beam_vals=(*beam_base[pol_i])[beam_i]
;    model_vals=(*model_img_holo[pol_i])[beam_i]
;    image_vals=(*dirty_img[pol_i])[beam_i]
;    scale_arr[pol_i]=(linfit(model_vals,image_vals,measure_error=1./beam_vals))[1]
;ENDFOR
  
IF Keyword_Set(file_path_galmodel) THEN save,model_img_holo,galaxy_model_img,galaxy_model_uv,model_uv_holo,filename=file_path_galmodel
IF Keyword_Set(uv_return) THEN RETURN,model_uv_holo ELSE RETURN,model_img_holo 
END