PRO fhd_multi,status_arr,fhd_file_list,source_array,comp_arr,fhd_params=fhd_params,obs_arr=obs_arr,$
    weights_arr=weights_arr,timing=timing,nside=nside,$
    residual_array=residual_array,dirty_uv_arr=dirty_uv_arr,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    silent=silent,beam_model=beam_model,beam_corr=beam_corr,norm_arr=norm_arr,source_mask=source_mask,hpx_inds=hpx_inds,$
    transfer_mapfn=transfer_mapfn,galaxy_model_fit=galaxy_model_fit,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  
t00=Systime(1)

IF N_Elements(obs_arr) EQ 0 THEN BEGIN
    n_obs=N_Elements(fhd_file_list)
    FOR obs_i=0,n_obs-1 DO BEGIN
        fhd_save_io,status_arr[obs_i],obs,var='obs',/restore,file_path_fhd=fhd_file_list[obs_i],_Extra=extra
        IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
        obs_arr[obs_i]=obs
    ENDFOR
ENDIF

n_obs=N_Elements(obs_arr)
;Note that defaults supplied here will be overwritten by any keywords passed through Extra
;use the same deconvolution parameters for all observations. obs is used for very little in here!
fhd_params=fhd_init(obs_arr[0],deconvolution_filter='filter_uv_uniform',max_deconvolution_components=Sqrt(n_obs)*10000.,$
    joint_deconvolution_list=fhd_file_list,_Extra=extra) 

jones_arr=Ptrarr(n_obs)
n_pol=fhd_params.npol
gain_factor=fhd_params.gain_factor
;gain_factor_use=gain_factor*(!RaDeg/(obs_arr.MAX_BASELINE/obs_arr.KPIX)/obs_arr.degpix)^2. ;correct by approx. beam area
max_iter=fhd_params.max_iter
max_deconvolution_components=fhd_params.max_deconvolution_components
check_iter=fhd_params.check_iter
beam_threshold=fhd_params.beam_threshold
beam_model_threshold=0.05
add_threshold=fhd_params.add_threshold
max_add_sources=fhd_params.max_add_sources
;local_max_radius=fhd_params.local_max_radius
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
beam_width=beam_width_calculate(obs_arr,min_restored_beam_width=1.,/FWHM)
local_max_radius=beam_width*2.
local_radius=local_max_radius*Mean(obs_arr.degpix)
source_alias_radius=Mean(obs_arr.degpix*obs_arr.dimension)/4.
calibration_model_subtract=fhd_params.cal_subtract
filter_background=fhd_params.filter_background
decon_filter=fhd_params.decon_filter

icomp=Complex(0,1)
beam_max_threshold=fhd_params.beam_max_threshold
smooth_width=fhd_params.smooth_width

beam_model=Ptrarr(n_pol,n_obs,/allocate)
beam_corr=Ptrarr(n_pol,n_obs,/allocate)
beam_mask_arr=Ptrarr(n_obs,/allocate)
source_mask_arr=Ptrarr(n_obs,/allocate)
beam_sourcefind_mask_arr=Ptrarr(n_obs,/allocate)
;beam_model_hpx_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
;weights_inv_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
map_fn_arr=Ptrarr(n_pol,n_obs)
dirty_uv_arr=Ptrarr(n_pol,n_obs,/allocate) 
model_uv_holo=Ptrarr(n_pol,n_obs,/allocate)
model_uv_full=Ptrarr(n_pol,n_obs,/allocate)
weights_arr=Ptrarr(n_pol,n_obs)
filter_arr=Ptrarr(n_pol,n_obs,/allocate)

;uv_mask_arr=Ptrarr(n_obs,/allocate)
comp_arr=Ptrarr(n_obs,/allocate)
hpx_cnv=Ptrarr(n_obs)
xv_arr=Ptrarr(n_obs,/allocate)
yv_arr=Ptrarr(n_obs,/allocate)
uv_i_arr=Ptrarr(n_obs,/allocate)

box_coords=Lonarr(n_obs,4)
norm_arr=Fltarr(n_pol,n_obs)
IF Keyword_Set(transfer_mapfn) THEN BEGIN
    IF N_Elements(transfer_mapfn) EQ 1 THEN BEGIN
        print,String(format='("Transferring mapfn from: ",A)',transfer_mapfn)
        FOR pol_i=0,n_pol-1 DO BEGIN
            fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=fhd_file_list[0],pol_i=pol_i,$
                transfer=transfer_mapfn,/no_save,path_use=path_use,obs=obs_arr[0],_Extra=extra
            RESTORE,path_use+'.sav' ;map_fn
;        print,'Restoring: ' + file_path_mapfn+pol_names[pol_i]+'.sav'
;        restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;map_fn
            map_fn=Ptr_new(Temporary(map_fn))
            FOR obs_i=0L,n_obs-1 DO map_fn_arr[pol_i,obs_i]=map_fn
        ENDFOR
    ENDIF ELSE BEGIN
        transfer_mapfn_uniq=transfer_mapfn[Uniq(transfer_mapfn,sort(transfer_mapfn))]
        n_mapfn=N_Elements(transfer_mapfn_uniq)
        FOR trans_map_i=0L,n_mapfn-1 DO BEGIN
            transfer_mapfn_use=transfer_mapfn_uniq[trans_map_i]
            FOR pol_i=0,n_pol-1 DO BEGIN
                fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=fhd_file_list[0],pol_i=pol_i,$
                    transfer=transfer_mapfn_use,/no_save,path_use=path_use,obs=obs_arr[0],_Extra=extra
                RESTORE,path_use+'.sav' ;map_fn
                map_fn=Ptr_new(Temporary(map_fn))
                obs_trans_i=where(transfer_mapfn EQ transfer_mapfn_use,n_obs_match)
                IF n_obs_match GT 0 THEN map_fn_arr[pol_i,obs_trans_i]=map_fn
            ENDFOR
        ENDFOR
    ENDELSE
ENDIF

obs_weight=Ptrarr(n_obs)
FOR obs_i=0L,n_obs-1 DO BEGIN
    file_path_fhd=fhd_file_list[obs_i]
    print,String(format='("Pre-processing obs ",A," of ",A," (",A,")")',Strn(obs_i+1),Strn(Long(n_obs)),file_basename(file_path_fhd))
    obs=obs_arr[obs_i]
    status_str=status_arr[obs_i]
;    fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    jones_arr[obs_i]=Ptr_new(Temporary(jones)) ;do NOT use undefine_fhd or some pointers will be null
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
    
    psf=beam_setup(obs,status_str,file_path_fhd=file_path_fhd,restore_last=1,silent=1,/no_save)
    FOR pol_i=0,n_pol-1 DO *beam_model[pol_i,obs_i]=Sqrt(beam_image(psf,pol_i=pol_i,dimension=obs.dimension,/square))
    undefine_fhd,psf
    beam_sourcefind_mask=(beam_mask=fltarr(obs.dimension,obs.elements)+1)
    
    
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=(mask1=fltarr(obs.dimension,obs.elements))
        ref_pix=Long(obs.obsx)+Long(obs.dimension)*Long(obs.obsy)
        mask_i=region_grow(*beam_model[pol_i,obs_i],ref_pix,thresh=[beam_threshold,max(*beam_model[pol_i,obs_i])])
        mask0[mask_i]=1
        beam_sourcefind_mask*=mask0
                
        mask_i=where(*beam_model[pol_i,obs_i] GE beam_model_threshold)
        mask1[mask_i]=1
        beam_mask*=mask1
    ENDFOR
    
    mask_i=(mask0=(mask1=0))
    *beam_sourcefind_mask_arr[obs_i]=beam_sourcefind_mask
    *beam_mask_arr[obs_i]=beam_mask
    *source_mask_arr[obs_i]=beam_mask
    FOR pol_i=0,n_pol-1 DO *beam_corr[pol_i,obs_i]=weight_invert(*beam_model[pol_i,obs_i]*beam_mask)

    ;supply beam_mask in case file is missing and needs to be generated
    hpx_cnv0=healpix_cnv_generate(obs,status_str,file_path_fhd=file_path_fhd,nside=nside_chk,$
        mask=beam_sourcefind_mask,restore_last=0) 
    IF N_Elements(nside) EQ 0 THEN nside=nside_chk
    IF nside_chk NE nside THEN hpx_cnv0=healpix_cnv_generate(obs,status_str,file_path_fhd=file_path_fhd,$
        nside=nside,mask=beam_sourcefind_mask,restore_last=0)
    hpx_cnv[obs_i]=Ptr_new(Temporary(hpx_cnv0))
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,grid_uv,var='grid_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        *dirty_uv_arr[pol_i,obs_i]=grid_uv
        *model_uv_full[pol_i,obs_i]=Complexarr(dimension,elements)
        *model_uv_holo[pol_i,obs_i]=Complexarr(dimension,elements)
    ENDFOR
    
    source_uv_mask=fltarr(dimension,elements)
    source_uv_mask2=fltarr(dimension,elements)
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF N_Elements(*map_fn_arr[pol_i,obs_i]) EQ 0 THEN BEGIN
            ;IMPORTANT: this approach of restoring the map_fn uses the least memory
            fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=file_path_fhd,pol_i=pol_i,$
                /no_save,path_use=path_use,obs=obs,_Extra=extra
            print,'Restoring: ' + path_use+'.sav' 
            RESTORE,path_use+'.sav' ;map_fn
            *map_fn_arr[pol_i,obs_i]=Temporary(map_fn)
        ENDIF
        weights_single=holo_mapfn_apply(complexarr(dimension,elements)+1,map_fn_arr[pol_i,obs_i],/no_conj,/indexed,_Extra=extra)
        weights_single_conj=Conj(Shift(Reverse(Reverse(weights_single,1),2),1,1))
        weights_arr[pol_i,obs_i]=Ptr_new((weights_single+weights_single_conj)/2.)
        source_uv_mask[where(*weights_arr[pol_i,obs_i])]=1.
        source_uv_mask2[where(weights_single)]=1.
        weights_single=(weights_single_conj=0)
    ENDFOR
    gain_normalization = get_image_renormalization(obs,weights_arr=weights_arr[*,obs_i],beam_base=beam_model[*,obs_i],$
        filter_arr=filter_arr[*,obs_i],image_filter_fn=decon_filter,degpix=obs.degpix,/antialias,file_path_fhd=file_path_fhd)
    gain_normalization *= (obs.degpix*!DtoR)^2. ; Convert images from Jy/sr to Jy/pixel
    
    *comp_arr[obs_i]=source_comp_init(n_sources=max_deconvolution_components,gain_factor=gain_factor)
    
    IF Keyword_Set(galaxy_model_fit) THEN BEGIN
        gal_model_uv=fhd_galaxy_model(obs,jones_arr[obs_i],file_path_fhd=file_path_fhd,/uv_return,_Extra=extra)
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_full[pol_i,obs_i]+=*gal_model_uv[pol_i]
            *model_uv_holo[pol_i,obs_i]=holo_mapfn_apply(*model_uv_full[pol_i,obs_i],map_fn_arr[pol_i,obs_i],_Extra=extra,/indexed)
        ENDFOR
        undefine_fhd,gal_model_uv
    ENDIF
;    *uv_mask_arr[obs_i]=source_uv_mask
    norm_arr[*,obs_i]=gain_normalization
    
    uv_i_use=where(Temporary(source_uv_mask),n_uv_use)
    uv_use_frac=Float(n_uv_use)/(dimension*elements)
    print,"Fractional uv coverage: ",uv_use_frac,", normalization: ",gain_normalization
    *uv_i_arr[obs_i]=where(Temporary(source_uv_mask2),n_uv_use2)
    *xv_arr[obs_i]=xvals[*uv_i_arr[obs_i]]
    *yv_arr[obs_i]=yvals[*uv_i_arr[obs_i]]
    
    box_coords[obs_i,0]=(Min(xvals[where(beam_mask)])+dimension/2.-smooth_width)>0
    box_coords[obs_i,1]=(Max(xvals[where(beam_mask)])+dimension/2.+smooth_width)<(dimension-1)
    box_coords[obs_i,2]=(Min(yvals[where(beam_mask)])+elements/2.-smooth_width)>0
    box_coords[obs_i,3]=(Max(yvals[where(beam_mask)])+elements/2.+smooth_width)<(elements-1)
    beam_mask=(xvals=(yvals=0))
ENDFOR
gain_factor_use=gain_factor;*norm_arr
print,"Gain normalization factors used: ",norm_arr

print,"Generating combined healpix map indices in sparse format"
;healpix indices are in sparse format. Need to combine them
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds,reverse_ind=reverse_inds)
n_hpx=N_Elements(hpx_inds)
n_hpx_full=nside2npix(nside)
degpix_hpx=Sqrt((4*!Pi*!Radeg^2.)/n_hpx_full)
print,"Nside: ",nside
print,"Effective resolution (degrees): ",degpix_hpx
print,String(format='("Total pixels: ",A," (sky fraction: ",A,")")',Strn(n_hpx),Strn(Float(n_hpx)/Float(n_hpx_full)))

pix2vec_ring,nside,hpx_inds,pix_coords
vec2ang,pix_coords,dec_hpx,ra_hpx,/astro

;set up effective beam^2 weighting
weight_hpx=obs_weight_healpix(obs_arr,beam_model,jones_arr,hpx_ind_map,hpx_cnv,hpx_inds,$
    obs_weight_arr=obs_weight,beam_mask_arr=beam_sourcefind_mask_arr,_Extra=extra)
weight_hpx_corr=weight_invert(weight_hpx)

;converge_check=Fltarr(Ceil(max_iter/check_iter))
converge_check=Fltarr(max_iter)

t1=0 ;generation of model_images and image_use for source detection
t2=0 ;source extraction
t3=0 ;fit the brightest source(s) to each polarization/etc...
t4=0 ;update model and run Holo mapping function
i2=0. & i3=0.
t0=Systime(1)

si=0L
source_mask_hpx=Fltarr(n_hpx)+1.
zero_ind=where(weight_hpx EQ 0,n_zero)
IF n_zero GT 0 THEN source_mask_hpx[zero_ind]=0

res_arr=Ptrarr(n_pol,/allocate)
;smooth_arr=Ptrarr(n_pol,n_obs,/allocate)
recalc_flag=Intarr(n_obs)+1
residual_stokes_hpx=Ptrarr(n_pol)
print,"Starting joint deconvolution loop"
FOR i=0L,max_iter-1 DO BEGIN 
    FOR pol_i=0,n_pol-1 DO residual_stokes_hpx[pol_i]=Ptr_new(Fltarr(n_hpx))
    res_stokes_arr=Ptrarr(n_obs,/allocate)
    FOR obs_i=0,n_obs-1 DO BEGIN
        FOR pol_i=0,n_pol-1 DO BEGIN
            t1_0=Systime(1)
            residual=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i],$
                degpix=obs_arr[obs_i].degpix,filter=filter_arr[pol_i,obs_i],/antialias,norm=norm_arr[pol_i,obs_i])
            
            t2_0a=Systime(1)
            t1+=t2_0a-t1_0
            IF Keyword_Set(filter_background) THEN BEGIN
                res_use=fltarr(size(residual,/dimension))
                res_smooth=Median(residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]$
                    *(*beam_corr[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]],smooth_width,/even)$
                    *(*beam_model[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]
                res_use[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]=$
                    residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]-res_smooth
                *res_arr[pol_i]=res_use
            ENDIF ELSE *res_arr[pol_i]=residual
            residual=0.
            
            t2_0b=Systime(1)
            t2+=t2_0b-t2_0a
        ENDFOR
        
        res_stokes=stokes_cnv(res_arr,jones_arr[obs_i],obs_arr[obs_i],beam=beam_model[*,obs_i],/square,_Extra=extra)
        *res_stokes_arr[obs_i]=*res_stokes[0]
        FOR pol_i=0,n_pol-1 DO (*residual_stokes_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(*res_stokes[pol_i]*(*obs_weight[obs_i]),hpx_cnv[obs_i])
        ptr_free,res_stokes
    ENDFOR
    
    source_find_hpx=*residual_stokes_hpx[0]*Sqrt(weight_hpx_corr>0.)
    FOR pol_i=0,n_pol-1 DO *residual_stokes_hpx[pol_i]*=weight_hpx_corr
    
    converge_check[i]=Stddev(source_find_hpx[where(source_mask_hpx)],/nan)
;    IF i EQ 0 THEN converge_check[0]=converge_check[0]
    t3_0=Systime(1)
    
;    ;detect sources
    comp_arr1=fhd_source_detect_healpix(obs_arr,jones_arr,fhd_params,source_find_hpx,residual_stokes_hpx=residual_stokes_hpx,$
        beam_model=beam_model,beam_mask_arr=beam_mask_arr,ra_hpx=ra_hpx,dec_hpx=dec_hpx,$
        source_mask_arr=source_mask_arr,recalc_flag=recalc_flag,n_sources=n_sources,gain_factor_use=gain_factor_use,$
        nside=nside,region_inds=region_inds,pix_coords=pix_coords,reverse_inds=reverse_inds,res_stokes_arr=res_stokes_arr,$
        source_mask_hpx=source_mask_hpx,si_start=si,_Extra=extra)
    
    n_src_use=(max_deconvolution_components-si-1.)<n_sources
    ;generate UV model from source list
    FOR obs_i=0L,n_obs-1 DO BEGIN
        IF ~Ptr_valid(comp_arr1[obs_i]) THEN BEGIN recalc_flag[obs_i]=0 & CONTINUE & ENDIF
        IF n_src_use EQ n_sources THEN comp_single=*comp_arr1[obs_i] ELSE BEGIN
            si_use=where((*comp_arr1[obs_i]).id LT max_deconvolution_components)
            comp_single=(*comp_arr1[obs_i])[si_use]
        ENDELSE
        (*comp_arr[obs_i])[comp_single.id]=comp_single
        source_dft_multi,obs_arr[obs_i],jones_arr[obs_i],comp_single,model_uv_full[*,obs_i],$
            xvals=*xv_arr[obs_i],yvals=*yv_arr[obs_i],uv_i_use=*uv_i_arr[obs_i]
    ENDFOR
    si+=n_src_use
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    
    ;apply HMF
    FOR obs_i=0L,n_obs-1 DO BEGIN
        IF recalc_flag[obs_i] EQ 0 THEN CONTINUE
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_holo[pol_i,obs_i]=$
                holo_mapfn_apply(*model_uv_full[pol_i,obs_i],map_fn_arr[pol_i,obs_i],/indexed,_Extra=extra);*norm_arr[obs_i]
        ENDFOR
    ENDFOR
    t4+=Systime(1)-t4_0
    
    IF (si+1) GE max_deconvolution_components THEN BEGIN
        i2+=1                                        
        t10=Systime(1)-t0
        conv_chk=Stddev(source_find_hpx[where(source_mask_hpx)],/nan)
        print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
        converge_check[i2]=conv_chk
        BREAK
    ENDIF
    
    ;check convergence
    t10=Systime(1)-t0
    conv_chk=Stddev(source_find_hpx[where(source_mask_hpx)],/nan)
    IF (Round(i mod check_iter) EQ 0) THEN BEGIN
        IF ~Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',i,si,t10,conv_chk))
    ENDIF
    IF i GT 1 THEN BEGIN
;        i2+=1
;        converge_check[i2]=conv_chk
        IF 2.*converge_check[i] GT Max(source_find_hpx[where(source_mask_hpx)]) THEN BEGIN
            print,StrCompress(String(format='("Break after iteration ",I," from low signal to noise after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
            converge_check=converge_check[0:i]
;            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        IF converge_check[i] GE Max(converge_check[((i-Ceil(Alog10(i)))>0):i-1]) THEN BEGIN ;add more tolerance for small variations
            print,StrCompress(String(format='("Break after iteration ",I," from lack of convergence after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
            converge_check=converge_check[0:i]
;            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
    ENDIF
    source_find_hpx=0.
    Ptr_free,residual_stokes_hpx,res_stokes_arr ;free memory, but only after the BREAK statements (for debugging)
ENDFOR

;condense clean components
residual_array=Ptrarr(n_pol,n_obs,/allocate)
source_array=Ptrarr(n_obs)
FOR obs_i=0L,n_obs-1 DO *comp_arr[obs_i]=(*comp_arr[obs_i])[0:si-1] ;truncate component list to include only components actually deconvolved
FOR obs_i=0L,n_obs-1 DO BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        *residual_array[pol_i,obs_i]=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i],$
            degpix=obs_arr[obs_i].degpix,filter=filter_arr[pol_i,obs_i],/antialias,norm=norm_arr[pol_i,obs_i])*(*beam_corr[pol_i,obs_i])
    ENDFOR
    res_stokes=stokes_cnv(residual_array[*,obs_i],jones_arr[obs_i],obs_arr[obs_i],beam=beam_model[*,obs_i],_Extra=extra)
    image_use=*res_stokes[0]*Sqrt(*obs_weight[obs_i])
    Ptr_free,res_stokes
    image_use-=Median(image_use,smooth_width)
    noise_map=Stddev(image_use[where(*beam_mask_arr[obs_i])],/nan)*weight_invert(*obs_weight[obs_i])
    comp_arr1=*comp_arr[obs_i]
    source_array1=Components2Sources(comp_arr1,obs,radius=(local_max_radius/2.)>0.5,noise_map=noise_map)
    source_array[obs_i]=Ptr_new(source_array1)
    
ENDFOR

t00=Systime(1)-t00
print,'Deconvolution timing [per iteration]'
print,String(format='("FFT:",A,"[",A,"]")',Strn(Round(t1)),Strn(Round(t1*100/i)/100.))
print,String(format='("Filtering:",A,"[",A,"]")',Strn(Round(t2)),Strn(Round(t2*100/i)/100.))
print,String(format='("DFT source modeling:",A,"[",A,"]")',Strn(Round(t3)),Strn(Round(t3*100/i)/100.))
print,String(format='("Applying HMF:",A,"[",A,"]")',Strn(Round(t4)),Strn(Round(t4*100/i)/100.))
undefine_fhd,map_fn_arr,hpx_cnv,hpx_ind_map,res_arr,smooth_arr,healpix_map,filter_arr,obs_weight,beam_mask_arr
undefine_fhd,source_mask_arr,beam_sourcefind_mask_arr
Ptr_free,residual_stokes_hpx,res_stokes_arr ;free memory
timing=[t00,t1,t2,t3,t4]
!except=except
END