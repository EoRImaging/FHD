FUNCTION pointer_copy,ptr_in

IF N_Elements(ptr_in) EQ 1 THEN ptr_out=Ptr_new() ELSE ptr_out=Ptrarr(size(ptr_in,/dimension))

FOR dim_i=0L,N_Elements(ptr_in)-1 DO IF Ptr_valid(ptr_in[dim_i]) THEN ptr_out[dim_i]=Ptr_new(*ptr_in[dim_i]) 
RETURN,ptr_out
END