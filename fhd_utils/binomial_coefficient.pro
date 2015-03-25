
;returns the binomial coefficient k of an expansion of order n. 
FUNCTION binomial_coefficient,n,k

IF N_Elements(k) EQ 0 THEN k=Findgen(n+1)

coeff=gamma(n+1)/(gamma(k+1)*gamma(n-k+1)) 
RETURN,coeff
END