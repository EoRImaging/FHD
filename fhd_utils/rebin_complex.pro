function rebin_complex, array, d1, d2, d3, d4, d5, d6, d7, d8

  dims = size(array,/dimension)
  type = size(array, /type)
  
  n_ds = N_Params() - 1
  
  if n_ds eq 0 then message, 'at least one dimension must be specified'

  if n_elements(d1) gt 1 then result_dims=d1 $
  else begin
     case n_ds of
        1: result_dims = d1
        2: result_dims = [d1, d2]
        3: result_dims = [d1, d2, d3]
        4: result_dims = [d1, d2, d3, d4]
        5: result_dims = [d1, d2, d3, d4, d5]
        6: result_dims = [d1, d2, d3, d4, d5, d6]
        7: result_dims = [d1, d2, d3, d4, d5, d6, d7]
        8: result_dims = [d1, d2, d3, d4, d5, d6, d7, d8]
     endcase
  endelse

  wh_n1 = where(dims ne 1, count_n1, complement = wh_1, ncomplement = count_1)
  if count_n1 eq 0 then message, 'input array is a scalar or 1-component array'
  if total(abs(dims[wh_n1] - result_dims[wh_n1])) ne 0 then message, 'this routine only replicates arrays as slices of a new array'

  extra_dim_mask = intarr(n_elements(result_dims))
  if count_1 gt 0 then begin
     perm1 = [wh_n1, wh_1]
     new_array = transpose(array, perm1)
     extra_dim_mask[wh_1] = 1
  endif else begin
     perm1 = indgen(n_elements(dims))
     new_array = array
  endelse

  if n_elements(result_dims) gt n_elements(dims) then begin
     extra_dim_mask[n_elements(dims):*]=1
     perm1 = [perm1, indgen(n_elements(result_dims) - n_elements(dims)) + n_elements(dims)]
  endif
  new_res_dims = result_dims[perm1]

  wh_extra = where(extra_dim_mask eq 1, count_extra)
  if count_extra eq 0 then message, 'this routine only replicates arrays as slices of a new array'

  u = (1. + fltarr(result_dims[wh_extra]))
  temp = make_array(new_res_dims, type=type)
  temp[*] = new_array[*] # u[*]


  perm2 = sort(perm1) 
  temp = transpose(temp, perm2)


  return, temp
end
