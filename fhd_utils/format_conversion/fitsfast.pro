PRO FitsFast,data,header,action=action,file_path=file_path,write=write,read=read,_EXTRA=extra
;action is either 'read' or 'write'

;IF not Keyword_Set(extension) THEN extension=0
ON_ERROR,2

UPNAME=StrUpCase(file_path)
pfits=strpos(UPNAME,'.FIT')
IF pfits EQ -1 THEN file_path_use=file_path+String('.fits') ELSE file_path_use=file_path
IF Keyword_Set(write) THEN action='write'
IF Keyword_Set(read) THEN action='read'
IF size(data,/type) EQ 8 THEN BEGIN
 dimension=N_tags(data)
 N_subdimensions=size(data.(0),/N_Dimensions)
 CASE N_subdimensions OF
  1: BEGIN
   elements=(size(data.(0),/Dimension))[0]
   array=fltarr(dimension,elements)
   FOR i=0.,dimension-1. DO array[i,*]=data.(i)
  END
  2: BEGIN
   elements_0=(size(data.(0),/Dimension))[0]
   elements_1=(size(data.(0),/Dimension))[1]
   array=fltarr(dimension,elements_0,elements_1)
   FOR i=0.,dimension-1. DO array[i,*,*]=data.(i)
  END
  0: BEGIN array=Fltarr(dimension) & FOR i=0.,dimension-1. DO array[i]=data.(i) & END
  ELSE: BEGIN print,'invalid structure type' & RETURN & END
 ENDCASE
 data_use=array
ENDIF ; ELSE IF action EQ 'write' THEN data_use=data

CASE action OF
 'read': BEGIN
  Fits_open,file_path_use,fcb,/no_abort
;  Fits_read,fcb,data,header, EXTEN_NO=extension
  Fits_read,fcb,data,header,_EXTRA=extra,/no_abort
  Fits_close,fcb,/no_abort
 END
 'write': BEGIN
  Fits_open,file_path_use,fcb,/write,/no_abort
  IF Keyword_Set(data_use) THEN Fits_write,fcb,data_use,header,_EXTRA=extra,/no_abort ELSE Fits_write,fcb,data,header,_EXTRA=extra,/no_abort
  Fits_close,fcb,/no_abort
 END
 ELSE: print,'either action="read" or action="write" must be set'
ENDCASE
END