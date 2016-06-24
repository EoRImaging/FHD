FUNCTION byte_convert, byte_arr, swap_byte_order=swap_byte_order

dims=Size(byte_arr,/dimension)
dims[0]/=4
float_arr=Fltarr(dims)

IF Keyword_Set(swap_byte_order) THEN  FOR i=0L,N_Elements(float_arr)-1 DO float_arr[i]=Float(Reverse(byte_arr[4*i:4*i+3]),0,1) $
    ELSE FOR i=0L,N_Elements(float_arr)-1 DO float_arr[i]=Float(byte_arr[4*i:4*i+3],0,1)

RETURN,float_arr
END