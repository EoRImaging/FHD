FUNCTION stokes_cnv,image_arr,beam_arr=beam_arr,p_map=p_map,p_corr=p_corr,inverse=inverse,square=square
;converts [xx,yy,{xy,yx}] to [I,Q,{U,V}] or [I,Q,{U,V}] to [xx,yy,{xy,yx}] if /inverse is set

n_pol=N_Elements(beam_arr)
beam_use=Ptrarr(n_pol,/allocate)
FOR ii=0L,n_pol-1 DO *beam_use[ii]=*beam_arr[ii]
IF Keyword_Set(square) THEN FOR ii=0L,n_pol-1 DO *beam_use[ii]=*beam_use[ii]^2.

IF N_Elements(p_map) EQ 0 THEN BEGIN
    p_map=Ptrarr(n_pol,/allocate) 
    p_map_free=1
    FOR ii=0L,n_pol-1 DO *p_map[ii]=1. 
ENDIF
IF N_Elements(p_corr) EQ 0 THEN BEGIN
    p_corr=Ptrarr(n_pol,/allocate) 
    p_corr_free
    FOR ii=0L,n_pol-1 DO *p_corr[ii]=1. 
ENDIF
IF Keyword_Set(inverse) THEN p_use=p_map ELSE p_use=p_corr

stokes_list1=[0,0,2,2]
stokes_list2=[1,1,3,3]
IF n_pol EQ 1 THEN stokes_list1=(stokes_list2=[0,0,0,0])
sign=[1,-1,1,-1]

type=size(image_arr,/type)
IF type EQ 8 THEN BEGIN ;check if a source list structure is supplied
    source_list=image_arr
    ns=N_Elements(source_list)
    sx=source_list.x
    sy=source_list.y
    pol_arr=Ptrarr(4,/allocate)
    flux_arr=Ptrarr(n_pol,/allocate)
    flux_out=Ptrarr(n_pol,/allocate)
    ;if the polarization map and beam models are supplied as vectors, assume they are already calculated for each component. Otherwise, assume they are 2D arrays the same size as the image
    IF size(*p_use[0],/n_dimension) GT 1 THEN FOR pol_i=0,n_pol-1 DO *pol_arr[pol_i]=(*p_use[pol_i])[sx,sy] ELSE FOR pol_i=0,n_pol-1 DO *pol_arr[pol_i]=*p_use[pol_i]
    IF size(*beam_use[0],/n_dimension) GT 1 THEN FOR pol_i=0,n_pol-1 DO *beam_use[pol_i]=(*beam_use[pol_i])[sx,sy]
    
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
        stokes_i_offset=0
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list.flux.(pol_i+4)
        
        FOR pol_i=0,n_pol-1 DO *flux_out[pol_i]=((*flux_arr[stokes_list1[pol_i]])+sign[pol_i]*(*flux_arr[stokes_list2[pol_i]]))*(*beam_use[pol_i])*(*p_use[pol_i])
    ENDIF ELSE BEGIN ;instrumental -> Stokes
        stokes_i_offset=4  
        FOR pol_i=0,n_pol-1 DO *flux_arr[pol_i]=source_list.flux.(pol_i)
        *flux_out[0]=(*flux_arr[stokes_list1[0]]+*flux_arr[stokes_list2[0]])*weight_invert((*beam_use[stokes_list2[0]]+*beam_use[stokes_list1[0]])/2.)
        FOR pol_i=1,n_pol-1 DO BEGIN
            *flux_out[pol_i]=(*flux_arr[stokes_list1[pol_i]])*(*beam_use[stokes_list1[pol_i]])*(*p_use[stokes_list1[pol_i]])+$
                sign[pol_i]*(*flux_arr[stokes_list2[pol_i]])*(*beam_use[stokes_list2[pol_i]])*(*p_use[stokes_list2[pol_i]])
        ENDFOR
    ENDELSE
    FOR pol_i=0,n_pol-1 DO source_list.flux.(pol_i+stokes_i_offset)=*flux_out[pol_i] ;indices of source_list.flux are [xx,yy,xy,yx,I,Q,U,V]
    
    Ptr_free,flux_out,flux_arr,pol_arr
    result=source_list
;    RETURN,source_list
ENDIF ELSE BEGIN
    n_pol=N_Elements(image_arr) ;redefine it here, just to make sure it matches the images
    image_arr_out=Ptrarr(n_pol)
    IF ~Ptr_valid(image_arr[0]) THEN RETURN,image_arr_out
    
    ;stokes I can have proper inverse-variance weighting. All other polarizations need to be converted to 'true sky' frame before they can be added
    IF Keyword_Set(inverse) THEN BEGIN ;Stokes -> instrumental
;        image_arr_out[0]=Ptr_new(((*image_arr[stokes_list1[pol_i]])*(*p_use[stokes_list1[pol_i]])+$
;            (*image_arr[stokes_list2[pol_i]]*(*p_use[stokes_list2[pol_i]])))*((*beam_use[stokes_list2[pol_i]])+(*beam_use[stokes_list1[pol_i]]))/2.)
        FOR pol_i=0,n_pol-1 DO image_arr_out[pol_i]=$
            Ptr_new(((*image_arr[stokes_list1[pol_i]])+sign[pol_i]*(*image_arr[stokes_list2[pol_i]]))*(*beam_use[pol_i])*(*p_use[pol_i]))
    ENDIF ELSE BEGIN ;instrumental -> Stokes
;        image_arr_out[0]=Ptr_new(((*image_arr[stokes_list1[0]])*(*p_use[stokes_list1[0]])+$
;            (*image_arr[stokes_list2[0]]*(*p_use[stokes_list2[0]])))*weight_invert((*beam_use[stokes_list2[0]])+(*beam_use[stokes_list1[0]]))/2.)
        image_arr_out[0]=Ptr_new((*image_arr[stokes_list1[0]]+*image_arr[stokes_list2[0]])*weight_invert((*beam_use[stokes_list2[0]]+*beam_use[stokes_list1[0]])/2.))
        FOR pol_i=1,n_pol-1 DO BEGIN
            image_arr_out[pol_i]=Ptr_new((*image_arr[stokes_list1[pol_i]])*(*beam_use[stokes_list1[pol_i]])*(*p_use[stokes_list1[pol_i]])+$
                sign[pol_i]*(*image_arr[stokes_list2[pol_i]])*(*beam_use[stokes_list2[pol_i]])*(*p_use[stokes_list2[pol_i]]))
        ENDFOR
    ENDELSE
    result=image_arr_out
;    RETURN,image_arr_out
ENDELSE

IF p_map_free THEN Ptr_Free,p_map
IF p_corr_free THEN Ptr_Free,p_corr
Ptr_Free,beam_use
RETURN,result
END