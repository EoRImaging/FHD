FUNCTION read_numpy,filename,data_type=data_type,fudge=fudge
filename='C:\MWA\DATA\X16\CASA\caltest\freq.npy'
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
;print,type_code_str

CASE type_code_str OF
    '|b1':type_code=1
    '<f8':type_code=4
    '<c16':type_code=6
    ELSE:BEGIN
        print,'type code could not be read! Assuming Int'
        type_code=2
    ENDELSE
ENDCASE

close,lun
free_lun,lun

head_len=(len+10+4)
head_len=Ceil(head_len/16.)*16
;print,head_len
data=read_binary(filename,data_type=type_code,data_dims=dims,endian='little',data_start=head_len+fudge)

;data=read_binary(lun,data_dims=dims,
RETURN,data
END