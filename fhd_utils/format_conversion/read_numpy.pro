;; The information used to parse .npy files comes from here: http://pyopengl.sourceforge.net/pydoc/numpy.lib.format.html

FUNCTION read_numpy, filename

  temp = strpos(filename, '.', /reverse_search)
  file_ext = strmid(filename, temp)

  if file_ext eq '.npz' then message, 'Only .npy files are supported, .npz files must be unzipped first.'
  if file_ext ne '.npy' then message, 'Only .npy files are supported.'

  openr,lun,filename,/get_lun
  header1=bytarr(10)
  readu,lun,header1
  len=Fix(header1[8])+256*Fix(header1[9])

  header=bytarr(len)
  readu,lun,header
  header=String(header)
  head_len=Strlen(header)

  ;; parse header to get dictionary parameters. Don't assume order of keys.
  ;; required dictionary keys:
  ;; 'descr': array's datatype
  ;; 'fortran_order': whether array is column or row ordered
  ;; 'shape': dimensions of the array
  ;; other keys will be ignored

  pos_shape=Strpos(header, "shape")
  if pos_shape eq 0 then message, 'no shape key in dictionary. Cannot parse file.'
  pos_dim_start = strpos(header, '(', pos_shape)
  pos_dim_end = strpos(header, ')', pos_dim_start)
  if pos_dim_start eq 0 or pos_dim_end eq 0 then message, 'Cannot parse shape key value'
  dims=Strsplit(Strmid(header,pos_dim_start,pos_dim_end-pos_dim_start),"':,() ",/extract)
  ;; shape=() means there is 1 element. After the strsplit this would give an empty string
  if n_elements(dims) eq 1 and dims[0] eq '' then dims = 1 else dims=Long(dims)

  pos_order=Strpos(header,"fortran_order")
  if pos_order eq 0 then message, 'no fortran_order key in dictionary. Cannot parse file.'
  pos_order_start = strpos(header, ':', pos_order)
  pos_order_end = strpos(header, ',', pos_order_start)
  if pos_order_start eq 0 or pos_order_end eq 0 then message, 'Cannot parse fortran_order key value'
  ordering = strsplit(strmid(header, pos_order_start, pos_order_end-pos_order_start), ":' ,",/extract)

  pos_type=Strpos(header,"descr")
  if pos_type eq 0 then message, 'no descr key in dictionary. Cannot parse file.'
  pos_type_start = strpos(header, ':', pos_type)
  pos_type_end = strpos(header, ',', pos_type_start)
  if pos_type_start eq 0 or pos_type_end eq 0 then message, 'Cannot parse descr key value'
  type_code_str=strsplit(strmid(header, pos_type_start, pos_type_end-pos_type_start), ":' ,",/extract)
  
  ;; fix dimensions for ordering if more than 1 dimension
  if n_elements(dims) gt 1 then begin
     case strlowcase(ordering) of
        'false': dims = reverse(dims)
        'true':
        else: print, 'ordering not recognized, assuming Fortran order'
     endcase
  endif

  ;; find data type:
  endian_type_code = strsplit(type_code_str, '[a-z0-9]+',/regex,/extract)
  data_type_code = strsplit(type_code_str, '[^a-z]',/regex,/extract)
  byte_length_code = strsplit(type_code_str, '[^0-9]',/regex,/extract)

  case endian_type_code of
     '<': endian = 'little'
     '>': endian = 'big'
     '=': endian = 'native'
     '|': begin
        if byte_length_code eq 1 then endian = 'native' else begin
           print, 'Endian type not recognized, using native'
           endian = 'native'
        endelse
     end
     else: begin
        print, 'Endian type not recognized, using native'
        endian = 'native'
     end
  endcase

  case data_type_code+byte_length_code of
     'f4': idl_type_code = 4 ;; idl float: 32 bit
     'f8': idl_type_code = 5 ;; idl double: 64 bit
     'c8': idl_type_code = 6 ;; idl complex: 2 x 32 bit
     'c16': idl_type_code = 9 ;; idl double complex: 2 x 64 bit
     'i2': idl_type_code = 2  ;; idl signed integer: 16 bit
     'i4': idl_type_code = 3  ;; idl signed long integer: 32 bit
     'i8': idl_type_code = 14 ;; idl signed long64 integer: 64 bit
     'u2': idl_type_code = 12 ;; idl unsigned integer: 16 bit
     'u4': idl_type_code = 13 ;; idl unsigned long integer: 32 bit
     'u8': idl_type_code = 15 ;; idl unsigned long64 integer: 64 bit
     'b1': idl_type_code = 1  ;; idl byte: 8 bit
     else: print, 'Data type not recognized, using byte array of appropriate length'
  endcase

  ;; figure out full header length -- round up to even factor of 16 bytes
  head_len=(len+10)
  head_len=Ceil(head_len/16.)*16  

  if n_elements(idl_type_code) eq 0 then begin
     data = bytarr(dims*byte_length_code)
     readu, lun, data
     free_lun,lun
  endif else begin
     free_lun,lun
     data=read_binary(filename,data_type=idl_type_code,data_dims=dims,endian=endian,data_start=head_len)
  endelse

  ;; fix dimensions for ordering
  if n_elements(dims) gt 1 and strlowcase(ordering) eq 'false' then data = transpose(data)

  RETURN,data
END
