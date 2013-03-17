PRO fhd_multi,fhd_file_list,obs_arr,image_uv_arr,source_array,comp_arr,weights_arr=weights_arr,timing=timing,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,transfer_mapfn=transfer_mapfn,$
    beam_base=beam_base,beam_correction=beam_correction,normalization=normalization,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  
t00=Systime(1)

IF N_Params() LT 2 THEN BEGIN
    n_obs=N_Elements(fhd_file_list)
    FOR obs_i=0,n_obs-1 DO BEGIN
        file_path=fhd_file_list[obs_i]
        restore,file_path+'_obs.sav'
        IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
        obs_arr[obs_i]=obs
    ENDFOR
ENDIF

n_obs=N_Elements(obs_arr)
fhd=fhd_init(obs_arr[0],_Extra=extra) ;use the same deconvolution parameters for all observations

n_pol=fhd.npol
baseline_threshold=fhd.baseline_threshold
gain_factor=fhd.gain_factor
mapfn_interval=fhd.mapfn_interval
max_iter=fhd.max_iter
max_sources=fhd.max_sources
check_iter=fhd.check_iter
mapfn_threshold=fhd.mapfn_threshold
beam_threshold=fhd.beam_threshold
add_threshold=fhd.add_threshold
max_add_sources=fhd.max_add_sources
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources
local_radius=local_max_radius*Mean(obs_arr.degpix)

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
smooth_width=fhd.smooth_width
pol_names=['xx','yy','xy','yx','I','Q','U','V']

beam=Ptrarr(n_pol,n_obs,/allocate)
beam_corr=Ptrarr(n_pol,n_obs,/allocate)
weights_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
;weights_inv_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
map_fn_arr=Ptrarr(n_pol,n_obs,/allocate)
dirty_uv_arr=Ptrarr(n_pol,n_obs,/allocate) 
model_uv_holo=Ptrarr(n_pol,n_obs,/allocate)
model_uv_full=Ptrarr(n_pol,n_obs,/allocate)

uv_mask_arr=Ptrarr(n_obs,/allocate)
comp_arr=Ptrarr(n_obs,/allocate)
ind_arr=Ptrarr(n_obs,/allocate)
hpx_cnv=Ptrarr(n_obs,/allocate)
xv_arr=Ptrarr(n_obs,/allocate)
yv_arr=Ptrarr(n_obs,/allocate)

box_coords=Lonarr(n_obs,4)

FOR obs_i=0.,n_obs-1 DO BEGIN
    file_path_fhd=fhd_file_list[obs_i]
    obs=obs_arr[obs_i]
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
    
    psf=beam_setup(obs,file_path_fhd,restore_last=1,silent=1)
    FOR pol_i=0,n_pol-1 DO *beam[pol_i,obs_i]=beam_image(psf,pol_i=pol_i,dimension=obs.dimension)
    
    beam_mask=fltarr(obs.dimension,obs.elements)+1
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(obs.dimension,obs.elements)
        mask_i=region_grow(*beam[pol_i,obs_i],obs.obsx+obs.dimension*obs.obsy,thresh=[0.05,max(*beam[pol_i,obs_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR
    FOR pol_i=0,n_pol-1 DO *beam_corr[pol_i,obs_i]=weight_invert(*beam[pol_i,obs_i]*beam_mask)

    ;supply beam_mask in case file is missing and needs to be generated
    *hpx_cnv[obs_i]=healpix_cnv_generate(obs,nside=nside,mask=beam_mask,radius=radius,restore_last=1) 
    
    source_comp_init,comp_arr0,n_sources=max_sources
    *comp_arr[obs_i]=comp_arr0
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        restore,filename=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav' ; dirty_uv,weights_grid
        *dirty_uv_arr[pol_i,obs_i]=dirty_uv*obs.cal[pol_i]
        *model_uv_full[pol_i,obs_i]=Complexarr(dimension,elements)
        *model_uv_holo[pol_i,obs_i]=Complexarr(dimension,elements)
        *weights_arr[pol_i,obs_i]=healpix_cnv_apply(*beam[pol_i,obs_i],*hpx_cnv[obs_i])
    ENDFOR
    
    source_uv_mask=fltarr(dimension,elements)
    FOR pol_i=0,n_pol-1 DO BEGIN
        restore,filename=file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav' ;map_fn
        *map_fn_arr[pol_i,obs_i]=map_fn
        weights_single=real_part(holo_mapfn_apply(complexarr(dimension,elements)+1,*map_fn_arr[pol_i,obs_i]))
        source_uv_mask[where(weights_single)]=1.
    ENDFOR
    *uv_mask_arr[obs_i]=source_uv_mask
    
    uv_i_use=where(source_uv_mask,n_uv_use)
    uv_use_frac=Float(n_uv_use)/(dimension*elements)
;    print,"Fractional uv coverage: ",uv_use_frac,"normalization: ",normalization
    *xv_arr[obs_i]=xvals[uv_i_use]
    *yv_arr[obs_i]=yvals[uv_i_use]
    
    box_coords[obs_i,0]=(Min((*xv_arr[obs_i])[where(beam_mask)])+dimension/2.-smooth_width)>0
    box_coords[obs_i,1]=(Max((*xv_arr[obs_i])[where(beam_mask)])+dimension/2.+smooth_width)<(dimension-1)
    box_coords[obs_i,2]=(Min((*yv_arr[obs_i])[where(beam_mask)])+elements/2.-smooth_width)>0
    box_coords[obs_i,3]=(Max((*yv_arr[obs_i])[where(beam_mask)])+elements/2.+smooth_width)<(elements-1)
ENDFOR

;healpix indices are in sparse format. Need to combine them
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds,full_ind_reference=full_ind_reference)
n_hpx_full=nside2npix(nside)
degpix_hpx=Sqrt((4*!Pi*!Radeg^2.)/n_hpx_full)

pix2vec_ring,nside,hpx_inds,pix_coords
vec2ang,pix_coords,dec_hpx,ra_hpx,/astro
    
converge_check=Fltarr(Ceil(max_iter/check_iter))
converge_check2=Fltarr(max_iter)

t1=0 ;generation of model_images and image_use for source detection
t2=0 ;source extraction
t3=0 ;fit the brightest source(s) to each polarization/etc...
t4=0 ;update model and run Holo mapping function
i2=0. & i3=0.
t0=Systime(1)

si=0L
healpix_map=Ptrarr(n_pol,/allocate)
weights_map=Ptrarr(n_pol,/allocate)
weights_corr_map=Ptrarr(n_pol,/allocate)
smooth_map=Ptrarr(n_pol,/allocate)
source_mask=Fltarr(n_hpx)+1.
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR obs_i=0,n_obs-1 DO BEGIN
        (*weights_map[pol_i])[*hpx_ind_map[obs_i]]+=*weights_arr[pol_i,obs_i]
    ENDFOR
    *weights_corr_map[pol_i]=weight_invert(*weights_map[pol_i])
ENDFOR
FOR pol_i=0,n_pol-1 DO source_mask[where(*weights_map[pol_i] EQ 0)]=0

FOR i=0L,max_iter-1 DO BEGIN 
    t1_0=Systime(1)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *healpix_map[pol_i]=Fltarr(n_hpx)
    ENDFOR
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
            *smooth_map[pol_i]=Fltarr(n_hpx)
            FOR obs_i=0,n_obs-1 DO BEGIN
                residual=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i])
                residual_hpx=healpix_cnv_apply(residual,*hpx_cnv[obs_i])
                (*healpix_map[pol_i])[*hpx_ind_map[obs_i]]+=residual_hpx
                IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
                    ;smooth image changes slowly and the median function takes a lot of time, so only re-calculate every few iterations
                    image_smooth=Median(residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]*$
                        (*beam_corr[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]],smooth_width,/even)*$
                        (*beam[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]
                    residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]-=image_smooth
                    smooth_hpx=healpix_cnv_apply(residual,*hpx_cnv[obs_i])
                    (*smooth_map[pol_i])[*hpx_ind_map[obs_i]]+=smooth_hpx
                ENDIF
            ENDFOR
        ENDIF
    ENDFOR
    
    t2_0=Systime(1)
    t1+=t2_0-t1_0
    ;NOTE healpix_map and smooth_hpx are in instrumental polarization, weighted by the beam squared
    
    ;convert to Stokes I
    source_find_hpx=(*healpix_map[0]-*smooth_map[0])*(*weights_corr_map[0])
    IF n_pol GT 1 THEN source_find_hpx+=(*healpix_map[1]-*smooth_map[1])*(*weights_corr_map[1])
    
    residual_I=(*healpix_map[0]-*smooth_map[0])*(*weights_corr_map[0])^2.
    IF n_pol GT 1 THEN residual_I+=(*healpix_map[1]-*smooth_map[1])*(*weights_corr_map[1])^2.
    
    converge_check2[i]=Stddev(source_find_hpx[where(source_mask)],/nan)
    
    ;detect sources
    flux_ref=Max(source_find_hpx*source_mask,max_i)
    flux_ref1=flux_ref*add_threshold
    source_i=where(source_find_hpx*source_mask GT flux_ref1,n_src)
    IF (n_src<max_add_sources)+si GT max_sources THEN max_add_sources=max_sources-(si+1)
    IF n_src GT max_add_sources THEN BEGIN
        source_list=source_find_hpx[source_i]
        source_i=source_i[(Reverse(Sort(source_list)))[0:max_add_sources-1]]
    ENDIF
    
    flux_arr=residual_I[source_i]
    ra_arr=fltarr(n_src)
    dec_arr=fltarr(n_src)
    FOR src_i=0L,n_src-1 DO BEGIN
        Query_disc,nside,pix_coords[source_i[src_i],*],local_radius,region_inds,ninds,/deg
        region_i=full_ind_reference[region_inds]
        region_i=region_i[where(region_i GE 0)] ;guaranteed at least the center pixel
        ra1=ra_hpx[region_i]
        dec1=dec_hpx[region_i]
        simg1=source_find_hpx[region_i]
        
        ra_arr[src_i]=Total(ra1*simg1)/Total(simg1)
        dec_arr[src_i]=Total(dec1*simg1)/Total(simg1)
    ENDFOR
    t3_0=Systime(1)
    t2+=t3_0-t2_0
    
    ;update models    
    FOR obs_i=0L,n_obs-1 DO BEGIN
        ad2xy,ra_arr,dec_arr,obs_arr[obs_i].astr,x_arr,y_arr
        comp_arr1=*comp_arr[obs_i]
        FOR src_i=0L,n_src-1 DO BEGIN
            si1=si+src_i
            
            FOR pol_i=0,n_pol-1 DO BEGIN   
                beam_corr_src[pol_i]=(*beam_correction[pol_i])[additional_i[src_i]]
                beam_src[pol_i]=(*beam_base[pol_i])[additional_i[src_i]]
                
                IF Keyword_Set(independent_fit) THEN BEGIN
                    sign=(pol_i mod 2) ? -1:1
                    IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]]+sign*image_use_Q[additional_i[src_i]]
                    IF pol_i GE 2 THEN flux_use=image_use_U[additional_i[src_i]]+sign*image_use_V[additional_i[src_i]]
                ENDIF ELSE IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]] ELSE flux_use=image_use_U[additional_i[src_i]]
                
                flux_use*=gain_factor_use/2.
                comp_arr1[si1].flux.(pol_i)=flux_use*beam_src[pol_i] ;Apparent brightness, instrumental polarization X gain (a scalar)
                flux_arr[pol_i]=flux_use;"True sky" instrumental pol
            ENDFOR
            
            comp_arr1[si1].flux.I=flux_arr[0]+flux_arr[1]
            comp_arr1[si1].flux.Q=flux_arr[0]-flux_arr[1]
            comp_arr1[si1].flux.U=flux_arr[2]+flux_arr[3]
            comp_arr1[si1].flux.V=flux_arr[2]-flux_arr[3]
            
            ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
            source_uv_vals=Exp(icomp*(2.*!Pi/dimension)*((comp_arr1[si1].x-dimension/2.)*xvals1+(comp_arr1[si1].y-elements/2.)*yvals1))
            FOR pol_i=0,n_pol-1 DO $
                (*model_uv_full[pol_i,obs_i])[uv_i_use]+=comp_arr1[si1].flux.(pol_i)*beam_corr_src[pol_i]*source_uv_vals
        ENDFOR
        comp_arr1=*comp_arr[obs_i]
    ENDFOR
    si+=n_src
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    
    ;apply HMF
    FOR obs_i=0L,n_obs-1 DO BEGIN
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_holo[pol_i,obs_i]=holo_mapfn_apply(*model_uv_full[pol_i,obs_i],*map_fn_arr[pol_i,obs_i],_Extra=extra)*normalization
        ENDFOR
    ENDFOR
    t4+=Systime(1)-t4_0
    
    IF si GE max_sources THEN BEGIN
        i2+=1                                        
        t10=Systime(1)-t0
        print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds (convergence:",F,")")',i,t10,Stddev((image_use*beam_avg)[where(source_mask)],/nan)))
        converge_check[i2]=Stddev((image_use*beam_avg)[where(source_mask)],/nan)
        BREAK
    ENDIF
    
    ;check convergence
    IF (Round(i mod check_iter) EQ 0) AND (i GT 0) THEN BEGIN
        i2+=1
        t10=Systime(1)-t0
        conv_chk=Stddev(source_find_hpx[where(source_find_hpx)],/nan)
        IF ~Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',i,si,t10,conv_chk))
        converge_check[i2]=conv_chk
        IF 2.*converge_check[i2] GT image_use[source_i] THEN BEGIN
            print,'Break after iteration',i,' from low signal to noise'
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        IF converge_check[i2] GT converge_check[i2-1] THEN BEGIN
            print,'Break after iteration',i,' from lack of convergence'
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
    ENDIF
ENDFOR
;condense clean components
noise_map=Stddev((image_use*beam_avg)[where(source_mask)],/nan)*weight_invert(beam_avg)
source_array=Components2Sources(comp_arr,radius=(local_max_radius/2.)>0.5,noise_map=noise_map)

FOR pol_i=0,n_pol-1 DO BEGIN
    *residual_array[pol_i]=dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i])*(*beam_correction[pol_i])
ENDFOR

t00=Systime(1)-t00
print,'Deconvolution timing [per iteration]'
print,String(format='("FFT:",A,"[",A,"]")',Strn(Round(t1)),Strn(Round(t1*100/i)/100.))
print,String(format='("Filtering:",A,"[",A,"]")',Strn(Round(t2)),Strn(Round(t2*100/i)/100.))
print,String(format='("DFT source modeling:",A,"[",A,"]")',Strn(Round(t3)),Strn(Round(t3*100/i)/100.))
print,String(format='("Applying HMF:",A,"[",A,"]")',Strn(Round(t4)),Strn(Round(t4*100/i)/100.))
timing=[t00,t1,t2,t3,t4]
!except=except
END