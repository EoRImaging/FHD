FUNCTION stokes_cnv,image_arr,jones,obs,beam_arr=beam_arr,inverse=inverse,square=square,no_extend=no_extend,$
    rotate_pol=rotate_pol,no_dipole_projection_rotation=no_dipole_projection_rotation,$
    center_rotate=center_rotate,debug_direction=debug_direction;,beam_threshold=beam_threshold
    ;/rotate_pol is a temporary debugging tool
;converts [xx,yy,{xy,yx}] to [I,Q,{U,V}] or [I,Q,{U,V}] to [xx,yy,{xy,yx}] if /inverse is set
;;Note that "image_arr" can actually be a 2D image, a vector of values, or a source_list structure. 
;requires the jones structure from fhd_struct_init_jones.pro

;Adjusts error handling so that if an error occurs, it halts execution and returns to the line at which the error occurred
ON_ERROR, 2

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
    IF Keyword_Set(jones) THEN BEGIN
        inds=jones.inds
        p_map=jones.Jmat
        p_corr=jones.Jinv
        dimension=jones.dimension
        elements=jones.elements
    ENDIF ELSE BEGIN
        IF size(image_arr,/type) EQ 8 THEN BEGIN
            dimension=obs.dimension
            elements=obs.elements
        ENDIF ELSE BEGIN
            dimension=(Size(*image_arr[0],/dimension))[0]
            elements=(Size(*image_arr[0],/dimension))[1]
        ENDELSE
        n_pix=dimension*elements
        p_map=Ptrarr(4,4,/allocate)
        FOR j=0,3 DO FOR i=0,3 DO *p_map[i,j]=Replicate(((i EQ j) ? 1.:0.),n_pix)
        p_corr=p_map
        p_free=1
    ENDELSE
ENDELSE
n_pix=N_Elements(inds)
IF Keyword_Set(inverse) THEN p_use=p_map ELSE p_use=p_corr

; Define the Stokes conversion:
; I = xx* + yy*
; Q = xx* - yy*
; U = xy* + yx*
; V = ixy* - iyx*
; where x is in the RA direction and y is in the Dec direction
stokes_mat_term1=[1,1,1,complex(0,1)]  ; Elements of the 4x4 Stokes conversion matrix, zeros removed
stokes_mat_term2=[1,-1,1,-complex(0,1)]
stokes_inv_term1=[.5,.5,.5,.5]
stokes_inv_term2=[.5,-.5,-complex(0,.5),complex(0,.5)]
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
        p_map[*,i+2]=jones.Jmat[*,3-i]
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
    IF ns EQ 0 THEN message, "Error: ns=0, probably no sources above the horizon"
    sx=sx[s_use]
    sy=sy[s_use]
    p_ind=p_ind[s_use]
    flux_arr=Ptrarr(n_pol,/allocate)
    flux_pq=Ptrarr(n_pol,/allocate)
    flux_out=Ptrarr(n_pol,/allocate)
    ;if the beam model is supplied as a vector, assume it is already calculated for each component. Otherwise, assume it is a 2D array the same size as the image
    IF size(*beam_use[0],/n_dimension) GT 1 THEN FOR pol_i=0,n_pol-1 DO *beam_use[pol_i]=(*beam_use[pol_i])[sx,sy]
    
    ;also convert extended source components.
    extend_i=where(Ptr_valid(source_list.extend),n_ext)
    IF Keyword_Set(no_extend) THEN n_ext=0
    FOR ext_i=0L,n_ext-1 DO *(source_list[extend_i[ext_i]].extend)=$
        stokes_cnv(*(source_list[extend_i[ext_i]].extend),jones,beam_arr=beam_arr,inverse=inverse,square=square,/no_extend)
    
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
        stokes_i_offset=0
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list[s_use].flux.(pol_i+4)
        FOR pol_i=0,n_pol-1 DO *flux_pq[pol_i]=(stokes_inv_term1[pol_i]*(*flux_arr[stokes_list1[pol_i]])+stokes_inv_term2[pol_i]*(*flux_arr[stokes_list2[pol_i]]))
        FOR pol_i2=0,n_pol-1 DO BEGIN
            IF pol_i2 LE 1 THEN *flux_out[pol_i2]=Fltarr(ns) ELSE *flux_out[pol_i2]=Complexarr(ns) 
            FOR pol_i1=0,n_pol-1 DO BEGIN
                *flux_out[pol_i2]+=*flux_pq[pol_i1]*(*p_map[pol_i1,pol_i2])[p_ind]
            ENDFOR
            *flux_out[pol_i2]*=*beam_use[pol_i2]
        ENDFOR
    ENDIF ELSE BEGIN ;instrumental -> Stokes
        stokes_i_offset=4  
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list[s_use].flux.(pol_i)
        
        FOR pol_i2=0,n_pol-1 DO BEGIN
            IF pol_i2 LE 1 THEN *flux_pq[pol_i2]=Fltarr(ns) ELSE *flux_pq[pol_i2]=Complexarr(ns)
            FOR pol_i1=0,n_pol-1 DO BEGIN
                *flux_pq[pol_i2]+=*flux_arr[pol_i1]*weight_invert(*beam_use[pol_i1])*(*p_corr[pol_i1,pol_i2])[p_ind]
            ENDFOR
        ENDFOR
        FOR pol_i=0,n_pol-1 DO *flux_out[pol_i]=Real_part(stokes_mat_term1[pol_i]*(*flux_pq[stokes_list1[pol_i]])+stokes_mat_term2[pol_i]*(*flux_pq[stokes_list2[pol_i]]))
    ENDELSE
     ;indices of source_list.flux are [xx,yy,xy,yx,I,Q,U,V]
    FOR pol_i=0,n_pol-1 DO BEGIN
        flux_single=fltarr(ns_in)
        IF pol_i+stokes_i_offset EQ 2 OR pol_i+stokes_i_offset EQ 3 THEN flux_single=Complexarr(ns_in) 
        flux_single[s_use]=*flux_out[pol_i]
        source_list.flux.(pol_i+stokes_i_offset)=flux_single ;;Reform() is to handle ns=1 case
    ENDFOR
    
    Ptr_free,flux_out,flux_arr,flux_pq
    result=source_list
ENDIF ELSE BEGIN ;else case is array of images
    n_pol=N_Elements(image_arr) ;redefine n_pol here, just to make sure it matches the images
    image_arr_out=Ptrarr(n_pol)
    IF ~Ptr_valid(image_arr[0]) THEN BEGIN
        Ptr_Free,beam_use
        RETURN,image_arr_out
    ENDIF
    dimension=(size(*image_arr[0],/dimension))[0]
    elements=(size(*image_arr[0],/dimension))[1]
    
    image_arr_sky=Ptrarr(n_pol)
    ;stokes I can have proper inverse-variance weighting. (not used!)
    ; All other polarizations need to be converted to 'true sky' frame before they can be added
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
        FOR sky_pol=0,n_pol-1 DO image_arr_sky[sky_pol]=$
            Ptr_new(stokes_inv_term1[sky_pol]*(*image_arr[stokes_list1[sky_pol]])+stokes_inv_term2[sky_pol]*(*image_arr[stokes_list2[sky_pol]]))
        FOR instr_pol=0,n_pol-1 DO BEGIN
            image_arr_out[instr_pol]=Ptr_new(fltarr(dimension,elements))
            FOR sky_pol=0,n_pol-1 DO BEGIN
                (*image_arr_out[instr_pol])[inds]+=(*image_arr_sky[sky_pol])[inds]*(*p_map[sky_pol,instr_pol])
            ENDFOR
            *image_arr_out[instr_pol]*=*beam_use[instr_pol]
        ENDFOR
        
    ENDIF ELSE BEGIN ;instrumental -> Stokes
        FOR sky_pol=0,n_pol-1 DO BEGIN
            image_arr_sky[sky_pol]=Ptr_new(Dcomplexarr(dimension,elements))
            FOR instr_pol=0,n_pol-1 DO BEGIN
                (*image_arr_sky[sky_pol])[inds]+=(*image_arr[instr_pol]*weight_invert(*beam_use[instr_pol]))[inds]*(*p_corr[instr_pol,sky_pol])
            ENDFOR
        ENDFOR
        FOR pol_i=0,n_pol-1 DO image_arr_out[pol_i]=$
            Ptr_new(Real_part(stokes_mat_term1[pol_i]*(*image_arr_sky[stokes_list1[pol_i]])+stokes_mat_term2[pol_i]*(*image_arr_sky[stokes_list2[pol_i]])))
    ENDELSE
    Ptr_free,image_arr_sky
    result=image_arr_out
ENDELSE

Ptr_Free,beam_use
IF Keyword_Set(p_free) THEN Ptr_Free,p_map,p_corr
RETURN,result
END
