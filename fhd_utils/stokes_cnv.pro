FUNCTION stokes_cnv,image_arr,jones,obs,beam_arr=beam_arr,inverse=inverse,square=square,no_extend=no_extend,$
    rotate_pol=rotate_pol,no_dipole_projection_rotation=no_dipole_projection_rotation,$
    center_rotate=center_rotate,debug_direction=debug_direction;,beam_threshold=beam_threshold
    ;/rotate_pol is a temporary debugging tool
;converts [xx,yy,{xy,yx}] to [I,Q,{U,V}] or [I,Q,{U,V}] to [xx,yy,{xy,yx}] if /inverse is set
;;Note that "image_arr" can actually be a 2D image, a vector of values, or a source_list structure. 
;requires the jones structure from fhd_struct_init_jones.pro

IF Min(Ptr_valid(beam_arr)) EQ 0 THEN BEGIN
    n_pol=4
    beam_use=Ptrarr(n_pol,/allocate)
    FOR ii=0L,n_pol-1 DO *beam_use[ii]=1.
ENDIF ELSE BEGIN
    n_pol=N_Elements(beam_arr)
    beam_use=pointer_copy(beam_arr)
    IF Keyword_Set(square) THEN FOR ii=0L,n_pol-1 DO *beam_use[ii]=*beam_use[ii]^2.
ENDELSE
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=1E-2
IF Keyword_Set(square) THEN beam_threshold_use=beam_threshold^2 ELSE beam_threshold_use=beam_threshold

IF size(jones,/type) EQ 10 THEN BEGIN
    inds=(*jones).inds
    p_map=(*jones).Jmat
    p_corr=(*jones).Jinv
    dimension=(*jones).dimension
    elements=(*jones).elements
ENDIF ELSE BEGIN
    inds=jones.inds
    p_map=jones.Jmat
    p_corr=jones.Jinv
    dimension=jones.dimension
    elements=jones.elements
ENDELSE
n_pix=N_Elements(inds)
IF Keyword_Set(inverse) THEN p_use=p_map ELSE p_use=p_corr

stokes_list1=[0,0,2,2]
stokes_list2=[1,1,3,3]
IF n_pol EQ 1 THEN stokes_list1=(stokes_list2=[0,0,0,0])

IF Keyword_Set(no_dipole_projection_rotation) THEN BEGIN
    ;this is meant as a debugging tool!
    p_map=Ptrarr(4,4,/allocate)
    FOR j=0,3 DO FOR i=0,3 DO *p_map[i,j]=Replicate(((i EQ j) ? 1.:0.),n_pix)
    p_corr=p_map
    p_free=1
ENDIF
IF Keyword_Set(debug_direction) THEN BEGIN
    p_store=p_map
    p_map=p_corr
    p_corr=p_store
ENDIF
IF Keyword_Set(rotate_pol) THEN BEGIN
    ;this is meant as a debugging tool!
    FOR i=0,1 DO BEGIN
        p_map[*,i]=jones.Jmat[*,1-i]
        p_corr[i,*]=jones.Jinv[1-i,*]
        p_corr[i+2,*]=jones.Jinv[3-i,*]
    ENDFOR
ENDIF
IF Keyword_Set(center_rotate) THEN BEGIN
    p_map=pointer_copy(p_map)
    p_corr=pointer_copy(p_corr)
    p_free=1
    IF N_Elements(obs) EQ 0 THEN BEGIN
        obsx=dimension/2.
        obsy=elements/2.
    ENDIF ELSE BEGIN
        obsx=obs.obsx
        obsy=obs.obsy
    ENDELSE
    inds_x=inds mod dimension
    inds_y=Floor(inds/dimension)
    ind_dist=sqrt((inds_x-obsx)^2.+(inds_y-obsy)^2.)
    min_dist=min(ind_dist,obs_ind_i)
    FOR i=0,3 DO BEGIN
        FOR j=0,3 DO BEGIN
            (*p_map[i,j])[*]=(*p_map[i,j])[obs_ind_i]
            (*p_corr[i,j])[*]=(*p_corr[i,j])[obs_ind_i]
        ENDFOR
    ENDFOR
ENDIF
sign=[1,-1,1,-1]

type=size(image_arr,/type)
IF type EQ 8 THEN BEGIN ;check if a source list structure is supplied
    source_list=image_arr
    ns_in=N_Elements(source_list)
    sx=source_list.x
    sy=source_list.y
    ind_arr=lonarr(dimension,elements)-1 ;set background to -1 to catch out of range pixels
    ind_arr[inds]=Lindgen(N_Elements(inds))
    p_ind=ind_arr[sx,sy]
    s_use=where(p_ind GE 0,ns)
    sx=sx[s_use]
    sy=sy[s_use]
    p_ind=p_ind[s_use]
    flux_arr=Ptrarr(n_pol,/allocate)
    flux_pq=Ptrarr(n_pol,/allocate)
    flux_out=Ptrarr(n_pol,/allocate)
    ;if the beam model is supplied as a vector, assume it is already calculated for each component. Otherwise, assume it is a 2D array the same size as the image
    IF size(*beam_use[0],/n_dimension) GT 1 THEN FOR pol_i=0,n_pol-1 DO *beam_use[pol_i]=(*beam_use[pol_i])[sx,sy]
    
    ;also convert extended source components. Set square=0 since the beam is already squared if that option is set
    extend_i=where(Ptr_valid(source_list.extend),n_ext)
    IF Keyword_Set(no_extend) THEN n_ext=0
    FOR ext_i=0L,n_ext-1 DO *(source_list[extend_i[ext_i]].extend)=$
        stokes_cnv(*(source_list[extend_i[ext_i]].extend),jones,beam_arr=beam_arr,inverse=inverse,square=square,/no_extend)
    
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
        stokes_i_offset=0
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list[s_use].flux.(pol_i+4)
        FOR pol_i=0,n_pol-1 DO *flux_pq[pol_i]=((*flux_arr[stokes_list1[pol_i]])+sign[pol_i]*(*flux_arr[stokes_list2[pol_i]]))/2.
        FOR pol_i2=0,n_pol-1 DO BEGIN
            *flux_out[pol_i2]=Fltarr(ns)
            FOR pol_i1=0,n_pol-1 DO BEGIN
                *flux_out[pol_i2]+=*flux_pq[pol_i1]*(*p_map[pol_i1,pol_i2])[p_ind]
            ENDFOR
            *flux_out[pol_i2]*=*beam_use[pol_i2]
        ENDFOR
    ENDIF ELSE BEGIN ;instrumental -> Stokes
        stokes_i_offset=4  
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list[s_use].flux.(pol_i)
        
        FOR pol_i2=0,n_pol-1 DO BEGIN
            *flux_pq[pol_i2]=fltarr(ns)
            FOR pol_i1=0,n_pol-1 DO BEGIN
                *flux_pq[pol_i2]+=*flux_arr[pol_i1]*weight_invert(*beam_use[pol_i1])*(*p_corr[pol_i1,pol_i2])[p_ind]
            ENDFOR
        ENDFOR
        FOR pol_i=0,n_pol-1 DO *flux_out[pol_i]=(*flux_pq[stokes_list1[pol_i]])+sign[pol_i]*(*flux_pq[stokes_list2[pol_i]])
    ENDELSE
     ;indices of source_list.flux are [xx,yy,xy,yx,I,Q,U,V]
    FOR pol_i=0,n_pol-1 DO BEGIN
        flux_single=fltarr(ns_in)
        flux_single[s_use]=*flux_out[pol_i]
        source_list.flux.(pol_i+stokes_i_offset)=flux_single ;;Reform() is to handle ns=1 case
    ENDFOR
    
    Ptr_free,flux_out,flux_arr,flux_pq
    result=source_list
ENDIF ELSE BEGIN
    n_pol=N_Elements(image_arr) ;redefine n_pol here, just to make sure it matches the images
    image_arr_out=Ptrarr(n_pol)
    IF ~Ptr_valid(image_arr[0]) THEN BEGIN
        Ptr_Free,beam_use
        RETURN,image_arr_out
    ENDIF
    dimension=(size(*image_arr[0],/dimension))[0]
    elements=(size(*image_arr[0],/dimension))[1]
    
    image_arr_pq=Ptrarr(n_pol)
    ;stokes I can have proper inverse-variance weighting. (not used!)
    ; All other polarizations need to be converted to 'true sky' frame before they can be added
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
        FOR pol_i=0,n_pol-1 DO image_arr_pq[pol_i]=$
            Ptr_new(((*image_arr[stokes_list1[pol_i]])+sign[pol_i]*(*image_arr[stokes_list2[pol_i]]))/2.)
        FOR pol_i2=0,n_pol-1 DO BEGIN
            image_arr_out[pol_i2]=Ptr_new(fltarr(dimension,elements))
            FOR pol_i1=0,n_pol-1 DO BEGIN
                (*image_arr_out[pol_i2])[inds]+=(*image_arr_pq[pol_i1])[inds]*(*p_map[pol_i1,pol_i2])
            ENDFOR
            *image_arr_out[pol_i2]*=*beam_use[pol_i2]
        ENDFOR
        
    ENDIF ELSE BEGIN ;instrumental -> Stokes
        image_arr_pq=Ptrarr(n_pol)
        FOR pol_i2=0,n_pol-1 DO BEGIN
            image_arr_pq[pol_i2]=Ptr_new(fltarr(dimension,elements))
            FOR pol_i1=0,n_pol-1 DO BEGIN
                (*image_arr_pq[pol_i2])[inds]+=(*image_arr[pol_i1]*weight_invert(*beam_use[pol_i1]))[inds]*(*p_corr[pol_i1,pol_i2])
            ENDFOR
        ENDFOR
        FOR pol_i=0,n_pol-1 DO image_arr_out[pol_i]=$
            Ptr_new((*image_arr_pq[stokes_list1[pol_i]])+sign[pol_i]*(*image_arr_pq[stokes_list2[pol_i]]))
    ENDELSE
    Ptr_free,image_arr_pq
    result=image_arr_out
ENDELSE

Ptr_Free,beam_use
IF Keyword_Set(p_free) THEN Ptr_Free,p_map,p_corr
RETURN,result
END