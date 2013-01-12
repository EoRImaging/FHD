FUNCTION meshgrid,dimension,elements,axis,integer=integer
;generates a 2D array of X or Y values
;set axis=1 for X values, axis=2 for Y values
IF N_params() EQ 2 THEN BEGIN
    axis=elements
    elements=dimension
ENDIF
IF axis EQ 2 THEN result=Floor(Lindgen(dimension,elements)/dimension) ELSE result=Lindgen(dimension,elements) mod dimension
IF not Keyword_Set(integer) THEN result=Float(result)

RETURN, result
END

