FUNCTION rebin_complex, arr, dim1, dim2, dim3, dim4, dim5, /sample

if N_Elements(sample) EQ 0 THEN sample=1

rea = real_part(arr)
ima = imaginary(arr)

CASE N_Params() OF
    3: result = Complex(Rebin(rea, dim1, sample),Rebin(ima, dim1, sample))
    4: result = Complex(Rebin(rea, dim1,dim2, /sample),Rebin(ima, dim1,dim2, sample))
    5: result = Complex(Rebin(rea, dim1,dim2,dim3, /sample),Rebin(ima, dim1,dim2,dim3, sample))
    6: result = Complex(Rebin(rea, dim1,dim2,dim3,dim4, /sample),Rebin(ima, dim1,dim2,dim3,dim4, sample))
    7: result = Complex(Rebin(rea, dim1,dim2,dim3,dim4,dim5, /sample),Rebin(ima, dim1,dim2,dim3,dim4,dim5, sample))
    
return, result

END
