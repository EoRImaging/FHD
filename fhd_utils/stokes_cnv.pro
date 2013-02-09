FUNCTION stokes_cnv,image_arr,beam_arr=beam_arr,p_map=p_map,p_corr=p_corr,inverse=inverse
;converts [xx,yy,{xy,yx}] to [I,Q,{U,V}] or [I,Q,{U,V}] to [xx,yy,{xy,yx}] if /inverse is set
dims=size(image_arr,/dimension)
n_pol=dims[0]
image_arr_out=Ptrarr(dims,/allocate)

IF not Keyword_Set(inverse) THEN BEGIN
    beam_use=Ptrarr(dims,/allocate)
    FOR ii=0L,Product(dims)-1 DO *beam_use[ii]=weight_invert(*beam_arr[ii])
    IF not Keyword_Set(p_corr) THEN BEGIN 
        p_use=Ptrarr(dims,/allocate) 
        FOR ii=0L,Product(dims)-1 DO *p_use[ii]=1. 
    ENDIF ELSE p_use=p_corr
ENDIF ELSE BEGIN
    beam_use=beam_arr
    IF not Keyword_Set(p_map) THEN BEGIN 
        p_use=Ptrarr(dims,/allocate) 
        FOR ii=0L,Product(dims)-1 DO *p_use[ii]=1. 
    ENDIF ELSE p_use=p_map
ENDELSE    

stokes_list1=[0,0,2,2]
stokes_list2=[1,1,3,3]
sign=[1,-1,1,-1]

IF n_pol EQ 1 THEN stokes_list1=(stokes_list2=[0,0,0,0])
FOR pol_i=0,n_pol-1 DO BEGIN
    *image_arr_out[pol_i]=(*image_arr[stokes_list1[pol_i]])*(*beam_use[stokes_list1[pol_i]])*(*p_use[stokes_list1[pol_i]])+$
        sign[pol_i]*(*image_arr[stokes_list2[pol_i]])*(*beam_use[stokes_list2[pol_i]])*(*p_use[stokes_list2[pol_i]])
ENDFOR
RETURN,image_arr_out
END