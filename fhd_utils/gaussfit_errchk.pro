; $Id: //depot/idl/IDL_71/idldir/lib/gaussfit.pro#1 $
;
; Copyright (c) 1982-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;

; NAME:
;   GAUSS_FUNCT
;
; PURPOSE:
;   EVALUATE THE SUM OF A GAUSSIAN AND A 2ND ORDER POLYNOMIAL
;   AND OPTIONALLY RETURN THE VALUE OF IT'S PARTIAL DERIVATIVES.
;   NORMALLY, THIS FUNCTION IS USED BY CURVEFIT TO FIT THE
;   SUM OF A LINE AND A VARYING BACKGROUND TO ACTUAL DATA.
;
; CATEGORY:
;   E2 - CURVE AND SURFACE FITTING.
; CALLING SEQUENCE:
;   FUNCT,X,A,F,PDER
; INPUTS:
;   X = VALUES OF INDEPENDENT VARIABLE.
;   A = PARAMETERS OF EQUATION DESCRIBED BELOW.
; OUTPUTS:
;   F = VALUE OF FUNCTION AT EACH X(I).
;
; OPTIONAL OUTPUT PARAMETERS:
;   PDER = (N_ELEMENTS(X),6) ARRAY CONTAINING THE
;       PARTIAL DERIVATIVES.  P(I,J) = DERIVATIVE
;       AT ITH POINT W/RESPECT TO JTH PARAMETER.
; COMMON BLOCKS:
;   NONE.
; SIDE EFFECTS:
;   NONE.
; RESTRICTIONS:
;   NONE.
; PROCEDURE:
;   F = A(0)*EXP(-Z^2/2) + A(3) + A(4)*X + A(5)*X^2
;   Z = (X-A(1))/A(2)
;   Elements beyond A(2) are optional.
; MODIFICATION HISTORY:
;   WRITTEN, DMS, RSI, SEPT, 1982.
;   Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;   Added to Gauss_fit, when the variable function name to
;       Curve_fit was implemented.  DMS, Nov, 1990.
;   CT, RSI, Dec 2003: Return correct array size if A[2] is 0.
;
PRO GAUSS_FUNCT,X,A,F,PDER
    COMPILE_OPT idl2, hidden
    ON_ERROR,2                      ;Return to caller if an error occurs
    n = n_elements(a)
    nx = N_ELEMENTS(x)

    if a[2] ne 0.0 then begin
        Z = (X-A[1])/A[2]   ;GET Z
        EZ = EXP(-Z^2/2.)   ;GAUSSIAN PART
    endif else begin
        z = REPLICATE(FIX(100, TYPE=SIZE(x,/TYPE)), nx)
        ez = z*0
    endelse

    case n of
        3:  F = A[0]*EZ
        4:  F = A[0]*EZ + A[3]
        5:  F = A[0]*EZ + A[3] + A[4]*X
        6:  F = A[0]*EZ + A[3] + A[4]*X + A[5]*X^2 ;FUNCTIONS.
    ENDCASE

    IF N_PARAMS(0) LE 3 THEN RETURN ;NEED PARTIAL?
;
    PDER = FLTARR(nx, n) ;YES, MAKE ARRAY.
    PDER[*,0] = EZ      ;COMPUTE PARTIALS
    if a[2] ne 0. then PDER[*,1] = A[0] * EZ * Z/A[2]
    PDER[*,2] = PDER[*,1] * Z
    if n gt 3 then PDER[*,3] = 1.
    if n gt 4 then PDER[*,4] = X
    if n gt 5 then PDER[*,5] = X^2
    RETURN
END



;+
; NAME:
;   GAUSSFIT
;
; PURPOSE:
;   Fit the equation y=f(x) where:
;
;       F(x) = A0*EXP(-z^2/2) + A3 + A4*x + A5*x^2
;           and
;       z=(x-A1)/A2
;
;   A0 = height of exp, A1 = center of exp, A2 = sigma (the width).
;   A3 = constant term, A4 = linear term, A5 = quadratic term.
;   Terms A3, A4, and A5 are optional.
;   The parameters A0, A1, A2, A3 are estimated and then CURVEFIT is
;   called.
;
; CATEGORY:
;   ?? - fitting
;
; CALLING SEQUENCE:
;   Result = GAUSSFIT(X, Y [, A])
;
; INPUTS:
;   X:  The independent variable.  X must be a vector.
;   Y:  The dependent variable.  Y must have the same number of points
;       as X.
;
; KEYWORD INPUTS:
;
;   CHISQ: Set this keyword to a named variable that will contain
;      the value of the chi-square goodness-of-fit.
;
;   ESTIMATES = optional starting estimates for the parameters of the
;       equation.  Should contain NTERMS (6 if NTERMS is not
;       provided) elements.
;
;   MEASURE_ERRORS: Set this keyword to a vector containing standard
;       measurement errors for each point Y[i].  This vector must be the same
;       length as X and Y.
;
;     Note - For Gaussian errors (e.g. instrumental uncertainties),
;        MEASURE_ERRORS should be set to the standard
;        deviations of each point in Y. For Poisson or statistical weighting
;        MEASURE_ERRORS should be set to sqrt(Y).
;
;   NTERMS = Set NTERMS to 3 to compute the fit: F(x) = A0*EXP(-z^2/2).
;      Set it to 4 to fit:  F(x) = A0*EXP(-z^2/2) + A3
;      Set it to 5 to fit:  F(x) = A0*EXP(-z^2/2) + A3 + A4*x
;
;   SIGMA: Set this keyword to a named variable that will contain
;      the 1-sigma error estimates of the returned parameters.
;
;     Note: if MEASURE_ERRORS is omitted, then you are assuming that
;           your model is correct. In this case, SIGMA is multiplied
;           by SQRT(CHISQ/(N-M)), where N is the number of points
;           in X and M is the number of terms in the fitting function.
;           See section 15.2 of Numerical Recipes in C (2nd ed) for details.
;
;   YERROR: The standard error between YFIT and Y.
;
; OUTPUTS:
;   The fitted function is returned.
;
; OPTIONAL OUTPUT PARAMETERS:
;   A:  The coefficients of the fit.  A is a three to six
;       element vector as described under PURPOSE.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   None.
;
; RESTRICTIONS:
;   The peak or minimum of the Gaussian must be the largest
;   or smallest point in the Y vector.
;
; PROCEDURE:
;   The initial estimates are either calculated by the below procedure
;   or passed in by the caller.  Then the function CURVEFIT is called
;   to find the least-square fit of the gaussian to the data.
;
;  Initial estimate calculation:
;   If NTERMS>=4 then a constant term is subtracted first.
;   If NTERMS>=5 then a linear term is subtracted first.
;   If the (MAX-AVG) of Y is larger than (AVG-MIN) then it is assumed
;   that the line is an emission line, otherwise it is assumed there
;   is an absorbtion line.  The estimated center is the MAX or MIN
;   element.  The height is (MAX-AVG) or (AVG-MIN) respectively.
;   The width is found by searching out from the extrema until
;   a point is found less than the 1/e value.
;
; MODIFICATION HISTORY:
;   DMS, RSI, Dec, 1983.
;   DMS, RSI, Jun, 1995, Added NTERMS keyword.  Result is now float if
;               Y is not double.
;   DMS, RSI, Added ESTIMATES keyword.
;   CT, RSI, Feb 2001: Change the way estimates are computed.
;         If NTERMS>3 then a polynomial of degree NTERMS-4 is subtracted
;         before estimating Gaussian coefficients.
;   CT, RSI, Nov 2001: Slight change to above modification:
;         Because a Gaussian and a quadratic can be highly correlated,
;         do not subtract off the quadratic term,
;         only the constant and linear terms.
;         Also added CHISQ, SIGMA and YERROR output keywords.
;   CT, RSI, May 2003: Added MEASURE_ERRORS keyword.
;   CT, RSI, March 2004: If estimate[2] is zero, compute a default value.
;   CT, ITTVIS, Sept 2008: Do all computations in double precision,
;       convert back to single precision if inputs were single precision.
;   IS, April 2015: pass error status out from curvefit.pro
;-
;
Function Gaussfit_errchk, xIn, yIn, a, $
    CHISQ=chisq, $
    ESTIMATES = est, $
    MEASURE_ERRORS=measureErrors, $
    NTERMS=nt, $
    SIGMA=sigma, $
    YERROR=yerror, $
    STATUS=status

    COMPILE_OPT idl2

    on_error,2                      ;Return to caller if an error occurs

    if n_elements(nt) eq 0 then nt = 6
    if nt lt 3 or nt gt 6 then $
       message,'NTERMS must have values from 3 to 6.'
    n = n_elements(yIn)       ;# of points.

    nMeas = N_ELEMENTS(measureErrors)
    if ((nMeas gt 0) && (nMeas ne n)) then $
        MESSAGE, 'MEASURE_ERRORS must be a vector of the same length as Y'

    nEst = n_elements(est)
    if (nEst && nEst ne nt) then $
        message, 'ESTIMATES must have NTERM elements.'

    isDouble = SIZE(xIn,/TYPE) eq 5 || SIZE(yIn,/TYPE) eq 5

    x = DOUBLE(xIn)
    y = DOUBLE(yIn)

    if (nEst eq 0 || est[2] eq 0) then begin  ;Compute estimates?

        if (nt gt 3) then begin
            ; For a Gaussian + polynomial, we need to subtract off either
            ; a constant or a straight line to get good estimates.
            ; NOTE: Because a Gaussian and a quadratic can be highly correlated,
            ; we do not want to subtract off the quadratic term.
            c = POLY_FIT(x, y, (nt eq 4) ? 0 : 1, yf)
            yd = y - yf
        endif else begin
            ; Just fitting a Gaussian. Don't need to subtract off anything.
            yd = y
            c = 0d
        endelse

        ;x,y and subscript of extrema
        ymax=max(yd, imax)
        xmax=x[imax]
        ymin=min(yd, imin)
        xmin=x[imin]
        if abs(ymax) gt abs(ymin) then i0=imax else i0=imin ;emiss or absorp?
        i0 = i0 > 1 < (n-2)     ;never take edges
        dy=yd[i0]           ;diff between extreme and mean
        del = dy/exp(1.)        ;1/e value
        i=0
        while ((i0+i+1) lt n) and $ ;guess at 1/2 width.
        ((i0-i) gt 0) and $
        (abs(yd[i0+i]) gt abs(del)) and $
        (abs(yd[i0-i]) gt abs(del)) do i=i+1
        a = [yd[i0], x[i0], abs(x[i0]-x[i0+i])]
        if nt gt 3 then a = [a, c[0]] ; estimate for constant term
        if nt gt 4 then a = [a, c[1]] ; estimate for linear term
        if nt gt 5 then a = [a, 0.]   ; assume zero for quadratic estimate
    endif

    ; Were estimates provided?
    if (nEst gt 0) then begin
        tmp = est
        ; Did we need to compute the A2 term above?
        if (est[2] eq 0) then $
            tmp[2] = a[2]
        a = tmp
    endif

    ; Convert from MEASURE_ERRORS to CURVEFIT weights argument.
    ; If we don't have MEASURE_ERRORS we will pass in an undefined variable
    ; to CURVEFIT, which will then assume no weighting.
    if (nMeas gt 0) then $
        weights = 1/measureErrors^2

    yfit = CURVEFIT(x,y,weights,a,sigma, $
        CHISQ=chisq, YERROR=yerror, $
        FUNCTION_NAME = "GAUSS_FUNCT", STATUS=STATUS) ;call curvefit

    if (~isDouble) then begin
        yfit = FLOAT(yfit)
        chisq = FLOAT(chisq)
        sigma = FLOAT(sigma)
        a = FLOAT(a)
    endif

    return, yfit
end
