FUNCTION rebin_complex, arr, dim1, dim2, dim3

rea = real_part(arr)
ima = imaginary(arr)

return, Complex(Rebin(rea, dim1, dim2, dim3, /sample),Rebin(ima, dim1,dim2,dim3, /sample))

END
