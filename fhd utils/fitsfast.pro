PRO FitsFast,data,header,action=action,filename=filename,filepathfull=filepathfull,RootDirectory=RootDirectory,$
    write=write,read=read,data_directory=data_directory,_EXTRA=extra
;action is either 'read' or 'write'
;by default, the file is written to or read from D:\CIBER
; filepathfull should take the format of, e.g. ['ciber','images','sim1raw']
;IF not Keyword_Set(extension) THEN extension=0
IF N_Elements(RootDirectory) EQ 0 THEN RootDirectory=rootdir()
ON_ERROR,2
UPNAME=StrUpCase(filename)

;Deal with obsolete keywords
CASE 1 OF 
    Keyword_Set(filepathfull):
    Keyword_Set(data_directory):filepathfull=data_directory
ENDCASE

pfits=strpos(UPNAME,'.FIT')
IF pfits EQ -1 THEN filename_use=filename+String('.fits') ELSE filename_use=filename
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
  IF N_Elements(Filepathfull) EQ 0 THEN IF strpos(filename,':') EQ 1 THEN Fits_open,filename_use,fcb,/no_abort ELSE $
    Fits_open,Filepath(filename_use,Root_dir=RootDirectory),fcb,/no_abort ELSE $
        Fits_open,Filepath(filename_use,Root_dir=RootDirectory,subdir=FilepathFull),fcb,/no_abort
;  Fits_read,fcb,data,header, EXTEN_NO=extension
  Fits_read,fcb,data,header,_EXTRA=extra,/no_abort
  Fits_close,fcb,/no_abort
 END
 'write': BEGIN
  IF N_Elements(Filepathfull) EQ 0 THEN IF strpos(filename,':') EQ 1 THEN Fits_open,filename_use,fcb,/write,/no_abort ELSE $
    Fits_open,Filepath(filename_use,Root_dir=RootDirectory),fcb,/write,/no_abort ELSE $
        Fits_open,Filepath(filename_use,Root_dir=RootDirectory,subdir=FilepathFull),fcb,/write,/no_abort
  IF Keyword_Set(data_use) THEN Fits_write,fcb,data_use,header,_EXTRA=extra,/no_abort ELSE Fits_write,fcb,data,header,_EXTRA=extra,/no_abort
  Fits_close,fcb,/no_abort
 END
 ELSE: print,'either action="read" or action="write" must be set'
ENDCASE
END