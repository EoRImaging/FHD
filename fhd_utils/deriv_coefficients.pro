FUNCTION deriv_coefficients,n,divide_factorial=divide_factorial
;computes an array of coefficients resulting in taking the n-th derivative of a function of the form x^a (a must not be a positive integer less than n)

IF n LE 0 THEN RETURN,0.
coeff=Fltarr(n)

coeff[0]=1.
FOR m=1.,n-1 DO coeff[1:m]+=-m*coeff[0:m-1]

IF Keyword_Set(divide_factorial) THEN FOR m=0,n-1 DO coeff[m]/=factorial(m+1)

RETURN,coeff
END