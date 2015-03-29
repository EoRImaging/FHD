FUNCTION deriv_coefficients,n
;computes an array of coefficients resulting in taking the n-th derivative of a function of the form x^a (a must not be a positive integer less than n)

coeff=Fltarr(n)

coeff[0]=1.
FOR m=1.,n-1 DO coeff[1:m]+=-m*coeff[0:m-1]

RETURN,coeff
END