FUNCTION Max_filter,Image,width,minimum=minimum,median=median,mean=mean,circle=circle,line_cut=line_cut,cross_cut=cross_cut,$
    full=full,kernel=kernel,edge_truncate=edge_truncate,mask=mask,missing=missing,radius=radius,stddev=stddev,fill=fill,double=double
;t=SYSTIME(1)
;set Keyword FILL=1 to only evaluate the function at masked points. Saves a lot of time if you are just filling in gaps!
IF N_Elements(edge_truncate) EQ 0 THEN edge_truncate=1
IF Keyword_Set(mask) THEN edge_truncate=1 ;there is no efficiency gained by skipping this if you use a mask
IF not Keyword_Set(missing) THEN missing=0.
IF Keyword_Set(radius) THEN width=2.*radius+1.
IF N_Elements(width) EQ 0 THEN width_use=5 ELSE width_use=width
IF width_use/2. EQ Floor(width_use/2.) THEN width_use+=1 ;always use an odd width
;Set Keyword minimum to return minimum values instead, same with median
;Keyword circle causes an octagonal kernel to be used
dimension=(size(Image,/dimension))[0]
elements=(size(Image,/dimension))[1]
IF (Keyword_Set(dimension) OR Keyword_Set(cross)) AND (N_Elements(width) EQ 0) THEN full=1
IF Keyword_Set(full) THEN BEGIN width_use=2.*(dimension>elements)+1 & edge_truncate=1 & ENDIF
WorkImage=Image
CASE 1 OF
    Keyword_Set(minimum):BEGIN fn='Min' & fn_i=1 & END
    Keyword_Set(median):BEGIN fn='Median' & fn_i=2 & END
    Keyword_Set(mean):BEGIN fn='Mean' & fn_i=3 & END
    Keyword_Set(stddev):BEGIN fn='Stddev' & fn_i=4 & END
    ELSE:BEGIN fn='Max' & fn_i=0 & END
ENDCASE

;edge=where(morph_distance(fltarr(dimension,elements),/back) LT width_use/2)

nan_i=where(Finite(WorkImage,/nan),n_nan)
IF n_nan GT 0 THEN WorkImage[nan_i]=Min(WorkImage[where(Finite(WorkImage))])
inf_i=where(Finite(WorkImage,/inf),n_inf)
IF n_inf GT 0 THEN WorkImage[inf_i]=Min(WorkImage[where(Finite(WorkImage))])

;Very slow way DELETED

;;stupid way: (fairly fast)
;Result=fltarr(dimension,elements)
;FOR j=0,elements-1 DO BEGIN
; FOR i=0,dimension-1 DO BEGIN
;   Result[i,j]=Max(WorkImage[((i-width_use/2)>0):((i+width_use/2-1)<dimension-1),((j-width_use/2)>0):((j+width_use/2-1)<elements-1)])
; ENDFOR
;ENDFOR

;Yet another way
;this method can make use of any shaped kernel, if one is supplied
CASE 1 OF
    Keyword_Set(kernel): BEGIN
        kernel_base=kernel
        width_use=(size(kernel,/dimension))[0]
    END
    Keyword_Set(circle) AND width_use GE 5 : BEGIN
        kernel_base=intarr(width_use,width_use)
        circle_template=(dist(width_use+1,width_use+1))
        circle_template=Shift(circle_template,1+width_use/2,1+width_use/2)
        circle_template=circle_template[1:*,1:*]
        kernel_base[where(circle_template LE width_use/2)]=1
    END
    Keyword_Set(line_cut): BEGIN
        kernel_base=intarr(width_use,width_use)
        IF line_cut EQ 1 THEN kernel_base[*,width_use/2]=1
        IF line_cut EQ 2 THEN kernel_base[width_use/2,*]=1
    END
    Keyword_Set(cross_cut): BEGIN
        kernel_base=intarr(width_use,width_use)
        kernel_base[*,width_use/2]=1
        kernel_base[width_use/2,*]=1
    END
    ELSE: kernel_base=intarr(width_use,width_use)+1
ENDCASE

xvals=(where(kernel_base) mod width_use)-Floor(width_use/2)
yvals=Floor(where(kernel_base)/width_use)-Floor(width_use/2)
yvals2=yvals*dimension
fast_vals=xvals+yvals*dimension
IF Keyword_Set(double) THEN Result=dblarr(dimension,elements) ELSE Result=fltarr(dimension,elements)
edge_test=fltarr(dimension,elements)
edge_width=Max([Abs(xvals),Abs(yvals)])
edge_test[0:edge_width,*]=1 & edge_test[dimension-edge_width-1:dimension-1,*]+=2
edge_test[*,0:edge_width]+=4 & edge_test[*,(elements-edge_width-1):elements-1]+=8

FOR j=0,elements-1 DO BEGIN
    FOR i=0,dimension-1 DO BEGIN
        IF Keyword_Set(fill) THEN IF mask[i,j] GT 0 THEN CONTINUE

        CASE edge_test[i,j] OF 
            0:values_indices=fast_vals+i+j*dimension
            1:BEGIN i_use=where(xvals+i GE 0) & values_indices=fast_vals[i_use]+i+j*dimension & END
            2:BEGIN i_use=where(xvals+i LT dimension) & values_indices=fast_vals[i_use]+i+j*dimension & END
            4:BEGIN i_use=where(yvals+j GE 0) & values_indices=fast_vals[i_use]+i+j*dimension & END
            5:BEGIN i_use=where((yvals+j GE 0) AND (xvals+i GE 0)) & values_indices=fast_vals[i_use]+i+j*dimension & END
            6:BEGIN i_use=where((yvals+j GE 0) AND (xvals+i LT dimension)) & values_indices=fast_vals[i_use]+i+j*dimension & END
            8:BEGIN i_use=where(yvals+j LT elements) & values_indices=fast_vals[i_use]+i+j*dimension & END
            9:BEGIN i_use=where((yvals+j LT elements) AND (xvals+i GE 0)) & values_indices=fast_vals[i_use]+i+j*dimension & END
            10:BEGIN i_use=where((yvals+j LT elements) AND (xvals+i LT dimension)) & values_indices=fast_vals[i_use]+i+j*dimension & END
        ENDCASE

        IF Keyword_Set(mask) THEN BEGIN
            i_use=where(mask[values_indices],n_use)
            IF n_use EQ 0 THEN BEGIN Result[i,j]=missing & CONTINUE & ENDIF
            values_indices=values_indices[i_use]
;            h_use=histogram(mask[values_indices],min=0,max=1,nbins=2,reverse_i=rev_i)
;            IF h_use[1] EQ 0 THEN Result[i,j]=missing & CONTINUE & ENDIF
;            IF h_use[0] GT 0 THEN values_indices=values_indices[rev_i[rev_i[
        ENDIF
            
        Result[i,j]=Call_function(fn,WorkImage[values_indices])
;        Result[i,j]=Median(values)
        
    ENDFOR
ENDFOR

IF Keyword_Set(fill) THEN BEGIN
    fill_i=where(mask EQ 0,n_fill)
    IF n_fill GT 0 THEN WorkImage[fill_i]=Result[fill_i]
    Result=WorkImage
ENDIF
;print,'time elapsed: ',Systime(1)-t
RETURN,Result
END