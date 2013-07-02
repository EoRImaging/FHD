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

  pos_dims=Strpos(header,"'shape':")
  dims=Strsplit(Strmid(header,pos_dims+10,head_len-1-(pos_dims+10)),',() }',/extract)
  dims=Long(dims)

  pos_order=Strpos(header,"'fortran_order':")
  pos_type=Strpos(header,"'descr':")
  type_code_str=Strsplit(Strmid(header,pos_type+10,pos_order-(pos_type+10)-1),"' ,",/extract)
  ;;print,type_code_str
   
  ;; find data type:
  endian_type_code = strsplit(type_code_str, '[a-z0-9]+',/regex,/extract)
  data_type_code = strsplit(type_code_str, '[^a-z]',/regex,/extract)
  byte_length_code = strsplit(type_code_str, '[^0-9]',/regex,/extract)

  head_len=(len+10)
  head_len=Ceil(head_len/16.)*16
  
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
     else: print, 'Data type not recognized, use byte array of appropriate length'
  endcase

  if n_elements(idl_type_code) eq 0 then begin
     data = bytarr(dims*byte_length_code)
     readu, lun, data
     free_lun,lun
  endif else begin
     free_lun,lun
     data=read_binary(filename,data_type=idl_type_code,data_dims=dims,endian=endian,data_start=head_len)
  endelse


  RETURN,data
END
