FUNCTION rebin_complex, arr, dim1, dim2, dim3, dim4, dim5, sample=sample

if N_Elements(sample) EQ 0 THEN sample=1

rea = real_part(arr)
ima = imaginary(arr)

CASE N_Params() OF
    2: result = Complex(Rebin(rea, dim1, sample=sample),Rebin(ima, dim1, sample=sample))
    3: result = Complex(Rebin(rea, dim1,dim2, sample=sample),Rebin(ima, dim1,dim2, sample=sample))
    4: result = Complex(Rebin(rea, dim1,dim2,dim3, sample=sample),Rebin(ima, dim1,dim2,dim3, sample=sample))
    5: result = Complex(Rebin(rea, dim1,dim2,dim3,dim4, sample=sample),Rebin(ima, dim1,dim2,dim3,dim4, sample=sample))
    6: result = Complex(Rebin(rea, dim1,dim2,dim3,dim4,dim5, sample=sample),Rebin(ima, dim1,dim2,dim3,dim4,dim5, sample=sample))
ENDCASE
return, result

END
