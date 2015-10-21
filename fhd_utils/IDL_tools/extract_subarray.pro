FUNCTION extract_subarray,array,xvals,yvals

dimension=Float((size(array,/dimension))[0])
sub_dim=N_Elements(xvals)
sub_elem=N_Elements(yvals)

inds=Rebin(xvals,sub_dim,sub_elem,/sample)+Rebin(transpose(yvals)*dimension,sub_dim,sub_elem)
sub_array=array[inds]

RETURN,sub_array
END