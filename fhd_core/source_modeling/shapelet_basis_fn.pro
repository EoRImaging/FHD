FUNCTION shapelet_basis_fn,xvals,yvals,major_axis,minor_axis,hermite_coeffs,n1,n2

;basis_fn=1./(major_axis*minor_axis)

xv_use=Double(xvals/major_axis)
yv_use=Double(yvals/minor_axis)

n_pix=N_Elements(xvals)
dimension=Sqrt(n_pix)
Vc_bar=Fltarr(dimension,dimension)
;M=Fltarr(n_pix,n1_max+1,n2_max+1)
;FOR n1=0L,n1_max DO BEGIN
;    FOR n2=0L,n2_max DO BEGIN
        D = sqrt((2.^(n1+n2))*!pi*major_axis*minor_axis*factorial(n1)*factorial(n2))
        C=exp(-0.5*(xv_use^2+yv_use^2))
        h1=Dblarr(dimension,dimension)
        FOR k=0L,n1 DO h1 +=hermite_coeffs[k,n1]*xv_use^(n1-k)
        h2=Dblarr(dimension,dimension)
        FOR k=0L,n2 DO h2 +=hermite_coeffs[k,n2]*yv_use^(n2-k)
        Vc_bar=(C/D)*h1*h2
;        FOR j=0,n_pix-1 DO BEGIN
;            C[j] = exp(-0.5*(xv_use[j]^2+yv_use[j]^2))
;            h1_ans=0D;
;            h2_ans=0D;
;        ; calculates the polynominal (co-effs in h1) at x=u or l
;            FOR k=0L,n1 DO h1_ans +=hermite_coeffs[n1, k]*xv_use[j]^(n1-k);
;        ;% calculates the polynomial (co-effs in h2) at x=v or m
;            FOR k=0L,n2 DO h2_ans +=hermite_coeffs[n2, k]*yv_use[j]^(n2-k);
;            Vc_bar[j] = C[j]/D*h1_ans*h2_ans;
;        ENDFOR
;        M[*,n1,n2]=Vc_bar
basis_fn=Vc_bar
;    ENDFOR
;ENDFOR
RETURN,basis_fn
END