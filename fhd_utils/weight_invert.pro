FUNCTION weight_invert,weights,threshold,abs=abs
result=fltarr(size(weights,/dimension))
IF Keyword_Set(abs) THEN BEGIN
    IF Keyword_Set(threshold) THEN i_use=where(Abs(weights) GE threshold,n_use) $
        ELSE i_use=where(Abs(weights),n_use)
    IF n_use GT 0 THEN result[i_use]=1./weights[i_use]
ENDIF ELSE BEGIN
    IF Keyword_Set(threshold) THEN i_use=where(weights GE threshold,n_use) $
        ELSE i_use=where(weights,n_use)
    IF n_use GT 0 THEN result[i_use]=1./weights[i_use]
ENDELSE

RETURN,result
END