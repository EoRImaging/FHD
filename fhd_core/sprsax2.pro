PRO sprsax2,A,X,B,X2=X2,B2=B2,A2=A2,double=double,transpose=transpose,mask=mask,complex=complex,indexed=indexed
;Major modification in the storage format of ija and sa which allows for much faster extraction of sub-arrays. The older format is still supported temporarily
;slight modification to sprsax to allow much larger arrays
;also modified to more efficiently use sparse vectors if mask is supplied 
; <<< MASK ONLY WORKS WITH TRANSPOSE!>>>
;set keyword "transpose" to use the transpose of A instead, without having to do extra calculations
;X2 and B2 are to allow efficient computation of the multiplication of two different vectors by the sparse matrix simultaneously
IF size(A,/type) EQ 10 THEN BEGIN ;check if pointer
    sa=(*A).sa
    ija=(*A).ija
    i_use=(*A).i_use
    n=N_Elements(i_use)
    IF tag_exist(*A,'indexed') THEN IF Keyword_Set((*A).indexed) THEN indexed=1 ELSE indexed=0
    IF tag_exist(*A,'norm') THEN norm=(*A).norm
ENDIF ELSE BEGIN
    sa=A.sa
    ija=A.ija
    i_use=A.i_use
    n=N_Elements(i_use)
    IF tag_exist(A,'indexed') THEN IF Keyword_Set(A.indexed) THEN indexed=1 ELSE indexed=0
    IF tag_exist(A,'norm') THEN norm=A.norm
ENDELSE

IF Keyword_Set(indexed) THEN BEGIN
    X_use=X[i_use]
    IF Keyword_Set(X2) THEN X2_use=X2[i_use]
ENDIF ELSE BEGIN
    X_use=X
    IF Keyword_Set(X2) THEN X2_use=X2
ENDELSE

;To use a double precision or complex B, supply it from the calling program
IF N_Elements(B) EQ 0 THEN BEGIN
    CASE 1 OF
        Keyword_Set(complex) AND Keyword_Set(double): B=Dcomplexarr(N_Elements(X))
        Keyword_Set(double): B=Dblarr(N_Elements(X))
        Keyword_Set(complex): B=Complexarr(N_Elements(X))
        ELSE: B=Fltarr(N_Elements(X))
    ENDCASE
ENDIF
IF Keyword_Set(X2) THEN BEGIN
    x2_flag=1 
    B2=B
ENDIF ELSE x2_flag=0
IF Keyword_Set(A2) THEN BEGIN
    a2_flag=1
    IF size(A2,/type) EQ 10 THEN BEGIN
        sa2=(*A2).sa 
        IF tag_exist(*A2,'norm') THEN norm2=(*A2).norm
    ENDIF ELSE BEGIN
        sa2=A2.sa 
        IF tag_exist(A2,'norm') THEN norm2=A2.norm
    ENDELSE
    B2=B
    x2_flag=0 ;not compatible to do both! Also, no time savings
ENDIF ELSE a2_flag=0
IF Keyword_Set(mask) THEN mask_flag=1 ELSE mask_flag=0

IF Keyword_Set(transpose) THEN BEGIN
    IF Keyword_Set(indexed) THEN BEGIN
        FOR i0=0L,n-1 DO BEGIN
            i=i0
            IF mask_flag THEN IF mask[i] EQ 0 THEN CONTINUE 
            B[i_use[*ija[i0]]]+=*sa[i0]*X_use[i]
            IF x2_flag THEN B2[*ija[i0]]+=*sa[i0]*X2_use[i]
            IF a2_flag THEN B2[*ija[i0]]+=*sa2[i0]*X_use[i]
        ENDFOR
    ENDIF ELSE BEGIN
        FOR i0=0L,n-1 DO BEGIN
            i=i_use[i0]
            IF mask_flag THEN IF mask[i] EQ 0 THEN CONTINUE 
            B[*ija[i0]]+=*sa[i0]*X_use[i]
            IF x2_flag THEN B2[*ija[i0]]+=*sa[i0]*X2_use[i]
            IF a2_flag THEN B2[*ija[i0]]+=*sa2[i0]*X_use[i]
        ENDFOR
    ENDELSE
ENDIF ELSE BEGIN
    FOR i0=0L,n-1 DO BEGIN
        i=i_use[i0]
        x1=X_use[*ija[i0]]
        B[i]=matrix_multiply(*sa[i0],x1,/atranspose)
        IF x2_flag THEN B2[i]=matrix_multiply(*sa[i0],X2_use[*ija[i0]],/atranspose)
        IF a2_flag THEN B2[i]=matrix_multiply(*sa2[i0],x1,/atranspose)
    ENDFOR
ENDELSE
    
IF Keyword_Set(norm) THEN BEGIN
    B/=norm
    IF x2_flag THEN B2/=norm
    IF a2_flag THEN B2/=norm2
ENDIF
END
    ;ENDIF ELSE BEGIN
    ;
    ;    sa=A.sa
    ;    ija=A.ija-1
    ;    
    ;    n=A.ija[0]-2L ;DO NOT include an extra -1 here. It MUST be ija[0]-2.
    ;    IF Keyword_Set(double) THEN B=dblarr(N_Elements(X)) ELSE B=Fltarr(N_Elements(X))
    ;;    IF Keyword_Set(double) THEN B=Dcomplexarr(N_Elements(X)) ELSE B=Complexarr(N_Elements(X))
    ;    
    ;    IF N_Params() LE 3 THEN BEGIN
    ;        FOR i=0L,n-1 DO BEGIN
    ;            B[i]=sa[i]*X[i]
    ;            i2=ija[i+1]-1
    ;            i1=ija[i]
    ;            IF i2 LT i1 THEN CONTINUE
    ;            sa_sub=sa[i1:i2]
    ;            ija_sub=ija[i1:i2]
    ;            B[i]+=Total(sa_sub*X[ija_sub],/double)
    ;        ENDFOR
    ;    ENDIF ELSE BEGIN
    ;        B2=B
    ;        FOR i=0L,n-1 DO BEGIN
    ;            B[i]=sa[i]*X[i]
    ;            B2[i]=sa[i]*X2[i]
    ;            i2=ija[i+1]-1
    ;            i1=ija[i]
    ;            IF i2 LT i1 THEN CONTINUE
    ;            sa_sub=sa[i1:i2]
    ;            ija_sub=ija[i1:i2]
    ;    ;        B[i]+=Total(sa_sub*X[ija_sub],/double)
    ;    ;        B2[i]+=Total(sa_sub*X2[ija_sub],/double)
    ;            
    ;            B[i]+=matrix_multiply(sa_sub,X[ija_sub],/atranspose)
    ;            B2[i]+=matrix_multiply(sa_sub,X2[ija_sub],/atranspose)
    ;        ENDFOR
    ;    ENDELSE
    ;ENDELSE
;    
;END