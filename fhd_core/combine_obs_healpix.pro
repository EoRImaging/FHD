PRO combine_obs_healpix,file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx=mrc_hpx,$
    nside=nside,restore_last=restore_last,output_path=output_path,beam_threshold=beam_threshold,$
    obs_arr=obs_arr,image_filter_fn=image_filter_fn,ston_cut=ston_cut,silent=silent,$
    catalog_file_path=catalog_file_path,restrict_hpx_inds=restrict_hpx_inds,_Extra=extra

except=!except
!except=0 
heap_gc
IF N_Elements(restrict_hpx_inds) EQ 0 THEN restrict_hpx_inds=0
IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_uniform'
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05
save_path=output_path+'_maps.sav'

IF Keyword_Set(restore_last) THEN BEGIN
    RESTORE,save_path
    RETURN
ENDIF
pol_names=['xx','yy','xy','yx']

;color_table=0.1
cal_ref_i=2
fix_flux=1
;combine_obs_sources,file_list,calibration,source_list,/restore_last,output_path=output_path

n_obs=N_Elements(file_list)

ftest=file_test(file_list+'_obs.sav') 
;ftest=intarr(n_obs)
;FOR file_i=0,n_obs-1 DO ftest[file_i]=file_test(file_list[file_i]+'_obs.sav')   
file_i_use=where(ftest,n_obs) 
file_list_use=file_list[file_i_use]
fhd_test=file_test(file_list_use+'_fhd.sav')
IF max(fhd_test) GT 0 THEN BEGIN
    fhd_flag=1
    IF min(fhd_test) EQ 0 THEN file_list_use=file_list_use[where(fhd_test)]
ENDIF ELSE fhd_flag=0

IF ~Keyword_Set(silent) THEN print,'Creating HEALPix maps'
FOR obs_i=0,n_obs-1 DO BEGIN
    file_path=file_list_use[obs_i]
    RESTORE,file_path+'_obs.sav'
    IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
    obs_arr[obs_i]=obs
    
    beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix);*(2.*Sqrt(2.*Alog(2.)))
    beam_area=2.*!Pi*(beam_width*obs.degpix)^2. 
    pix_sky=4.*!Pi*!RaDeg^2./beam_area
    Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
    Nside_chk*=2.
    
    IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
    nside_use=nside_use>Nside_chk
ENDFOR

n_pol=Min(obs_arr.n_pol)
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use

instr_model_hpx=Ptrarr(n_pol)
instr_dirty_hpx=Ptrarr(n_pol)
instr_sources_hpx=Ptrarr(n_pol)
instr_rings_hpx=Ptrarr(n_pol)
instr_catalog_hpx=Ptrarr(n_pol)
weights_hpx=Ptrarr(n_pol)
IF N_Elements(restrict_hpx_inds) GT 1 THEN BEGIN
    hpx_inds=restrict_hpx_inds
    n_hpx=N_Elements(restrict_hpx_inds)
    fit_inds_flag=0
ENDIF ELSE fit_inds_flag=1

FOR obs_i=0L,n_obs-1 DO BEGIN
    file_path_fhd=file_list_use[obs_i]
    IF ~Keyword_Set(silent) THEN print,StrCompress('Converting '+file_basename(file_path_fhd)+'('+Strn(obs_i+1)+' of '+Strn(n_obs)+')')
    obs=obs_arr[obs_i]
    dimension=obs.dimension
    elements=obs.elements
    n_vis_rel=obs.n_vis/Mean(obs_arr.n_vis)
    astr=obs.astr            
    restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
;    xvals=meshgrid(dimension,elements,1)-dimension/2
;    yvals=meshgrid(dimension,elements,2)-elements/2
    IF file_test(file_path_fhd+'_cal.sav') THEN cal=getvar_savefile(file_path_fhd+'_cal.sav','cal') ELSE cal=vis_struct_init_cal(obs,file_path_fhd=file_path_fhd)
    
    
    IF fhd_flag THEN BEGIN
        fhd=getvar_savefile(file_path_fhd+'_fhd_params.sav','fhd')
        ;'*_fhd.sav' contains: residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,weights_arr,beam_base,beam_correction,ra_arr,dec_arr,astr
        RESTORE,file_path_fhd+'_fhd.sav' 
        beam_base2=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base2[pol_i]=Ptr_new((*beam_base[pol_i])^2.)
        model_flag=1
        source_flag=1
    ENDIF ELSE BEGIN
        image_uv_arr=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO image_uv_arr[pol_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','dirty_uv',/pointer)
        weights_arr=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO weights_arr[pol_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','weights_grid',/pointer)
        model_uv_holo=Ptrarr(n_pol)
        IF min(file_test) GT 0 THEN model_flag=1 ELSE model_flag=0 
        IF model_flag THEN FOR pol_i=0,n_pol-1 DO model_uv_holo[pol_i]=getvar_savefile(file_path_fhd+'_uv_model_'+pol_names[pol_i]+'.sav','model_uv',/pointer)
        IF cal.n_cal_src GT 0 THEN BEGIN
            source_flag=1
            source_array=cal.source_list
        ENDIF ELSE source_flag=0
        
        IF file_test(file_path_fhd+'_beams.sav') THEN psf=getvar_savefile(file_path_fhd+'_beams.sav','psf') $
            ELSE psf=beam_setup(obs,file_path_fhd,restore_last=0,silent=1,no_save=1,_Extra=extra)
        beam_base=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base[pol_i]=Ptr_new(beam_image(psf,obs,pol_i=pol_i,square=0))
        beam_base2=Ptrarr(n_pol)
        FOR pol_i=0,n_pol-1 DO beam_base2[pol_i]=Ptr_new((*beam_base[pol_i])^2.)
    ENDELSE
    beam_mask=fltarr(dimension,elements)+1.
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(dimension,elements)
        mask_i=region_grow(*beam_base[pol_i],Floor(obs.obsx)+dimension*Floor(obs.obsy),thresh=[beam_threshold,max(*beam_base[pol_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR
    
    instr_model_arr=Ptrarr(n_pol)
    instr_dirty_arr=Ptrarr(n_pol)
    instr_sources=Ptrarr(n_pol)
    instr_rings=Ptrarr(n_pol)
    filter_arr=Ptrarr(n_pol,/allocate) 
    FOR pol_i=0,n_pol-1 DO BEGIN
        instr_dirty_arr[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
            image_filter_fn=image_filter_fn,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],_Extra=extra))
        IF model_flag THEN instr_model_arr[pol_i]=Ptr_new(dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
            image_filter_fn=image_filter_fn,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],_Extra=extra))
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN instr_rings[pol_i]=Ptr_new(source_image_generate(source_array,obs,pol_i=pol_i,resolution=16.,$
                dimension=dimension,restored_beam_width=restored_beam_width,ring_radius=ring_radius,_Extra=extra))
            instr_sources[pol_i]=Ptr_new(source_image_generate(source_array,obs,pol_i=pol_i,resolution=16.,$
                dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra))
        ENDIF
    ENDFOR
    
    ; renormalize based on weights
    renorm_factor = get_image_renormalization(obs,weights_arr=weights_arr,beam_base=beam_base,filter_arr=filter_arr,$
      image_filter_fn=image_filter_fn,degpix=degpix,/antialias)
    FOR pol_i=0,n_pol-1 DO BEGIN
      *instr_dirty_arr[pol_i]*=renorm_factor
      IF model_flag THEN *instr_model_arr[pol_i]*=renorm_factor 
    ENDFOR
    
    hpx_cnv=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,nside=nside,mask=beam_mask,restore_last=0,restrict_hpx_inds=restrict_hpx_inds,/no_save,_Extra=extra)
    hpx_inds1=hpx_cnv.inds 
    
    IF fit_inds_flag THEN BEGIN
        IF N_Elements(hpx_inds) EQ 0 THEN BEGIN
            hpx_inds=Temporary(hpx_inds1)
            IF Keyword_Set(restrict_hpx_inds) THEN BEGIN
                restrict_hpx_inds=hpx_inds
                fit_inds_flag=0
            ENDIF
            N_hpx=N_Elements(hpx_inds)
            ind_map1=Lindgen(N_hpx) ;index map of hpx_inds that correspond to hpx_inds1 
            reform_flag=0
        ENDIF ELSE BEGIN
            hist_min=Min(hpx_inds)<Min(hpx_inds1)
            hist_max=Max(hpx_inds)>Max(hpx_inds1)
            hist0=histogram(Temporary(hpx_inds),min=hist_min,max=hist_max,/binsize,reverse_ind=ri0)
            hist1=histogram(Temporary(hpx_inds1),min=hist_min,max=hist_max,/binsize,reverse_ind=ri1)
            hist=hist0+hist1
            hpx_inds_i=where(Temporary(hist),n_hpx)
            hist0=hist0[hpx_inds_i]
            hist1=hist1[hpx_inds_i]
            ind_use0=where(Temporary(hist0),n_hpx0)
            ind_use1=where(Temporary(hist1),n_hpx1)
            
            IF n_hpx0 EQ n_hpx THEN reform_flag=0 ELSE BEGIN
                reform_flag=1
                ind_order0=Sort(ri0[ri0[hpx_inds_i[ind_use0]]])
                ind_map0=ind_use0[ind_order0] 
                ri0=0               
            ENDELSE
            ind_order1=Sort(ri1[ri1[hpx_inds_i[ind_use1]]])
            ri1=0
            ind_map1=ind_use1[Temporary(ind_order1)]
            hpx_inds=(hpx_inds_i)+hist_min
        ENDELSE
    ENDIF ELSE BEGIN
    ;This option is not debugged!
        hist_min=Min(hpx_inds)<Min(hpx_inds1)
        hist_max=Max(hpx_inds)>Max(hpx_inds1)
        hist0=histogram(Temporary(hpx_inds),min=hist_min,max=hist_max,/binsize)
        hist1=histogram(Temporary(hpx_inds1),min=hist_min,max=hist_max,/binsize,reverse_ind=ri1)
;        hist=hist0+hist1 ; in this case, ONLY want indices found in the input hpx_inds
        hpx_inds_i=where(hist0 AND hist1,n_hpx)
        hist1=hist1[hpx_inds_i]
        ind_use1=where((hist1),n_hpx1)
        ind_order1=Sort(ri1[ri1[hpx_inds_i[ind_use1]]])
        ind_map1=ind_use1[ind_order1]
        reform_flag=0
    ENDELSE
    
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF ~Ptr_valid(instr_dirty_hpx[pol_i]) THEN instr_dirty_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF model_flag THEN IF ~Ptr_valid(instr_model_hpx[pol_i]) THEN instr_model_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN IF ~Ptr_valid(instr_rings_hpx[pol_i]) THEN instr_rings_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
            IF ~Ptr_valid(instr_sources_hpx[pol_i]) THEN instr_sources_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        ENDIF
        IF ~Ptr_valid(weights_hpx[pol_i]) THEN weights_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
        IF reform_flag THEN BEGIN
            instr_dirty_hpx0=Fltarr(n_hpx)
            instr_dirty_hpx0[ind_map0]=Temporary(*instr_dirty_hpx[pol_i])
            *instr_dirty_hpx[pol_i]=Temporary(instr_dirty_hpx0)
            
            IF model_flag THEN BEGIN
                instr_model_hpx0=Fltarr(n_hpx)
                instr_model_hpx0[ind_map0]=Temporary(*instr_model_hpx[pol_i])
                *instr_model_hpx[pol_i]=Temporary(instr_model_hpx0)
            ENDIF
            
            IF source_flag THEN BEGIN
                IF Keyword_Set(ring_radius) THEN BEGIN 
                    instr_rings_hpx0=Fltarr(n_hpx)
                    instr_rings_hpx0[ind_map0]=Temporary(*instr_rings_hpx[pol_i])
                    *instr_rings_hpx[pol_i]=Temporary(instr_rings_hpx0)
                ENDIF
                instr_sources_hpx0=Fltarr(n_hpx)
                instr_sources_hpx0[ind_map0]=(*instr_sources_hpx[pol_i])
                *instr_sources_hpx[pol_i]=Temporary(instr_sources_hpx0)
            ENDIF
            weights_hpx0=Fltarr(n_hpx)
            weights_hpx0[ind_map0]=Temporary(*weights_hpx[pol_i])
            *weights_hpx[pol_i]=Temporary(weights_hpx0)
        ENDIF 
        (*instr_dirty_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_dirty_arr[pol_i],hpx_cnv)
        IF model_flag THEN (*instr_model_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_model_arr[pol_i],hpx_cnv)
        IF source_flag THEN BEGIN
            IF Keyword_Set(ring_radius) THEN (*instr_rings_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_rings[pol_i],hpx_cnv)
            (*instr_sources_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*instr_sources[pol_i],hpx_cnv)
        ENDIF
        (*weights_hpx[pol_i])[ind_map1]+=healpix_cnv_apply(*beam_base2[pol_i],hpx_cnv)
    ENDFOR
    
    undefine_fhd,instr_model_arr,instr_dirty_arr,instr_sources,instr_rings,filter_arr,hpx_cnv,beam_base2,beam_base
ENDFOR

SAVE,hpx_inds,nside,obs_arr,instr_dirty_hpx,instr_model_hpx,weights_hpx,instr_sources_hpx,instr_rings_hpx,instr_catalog_hpx,filename=save_path,/compress
END