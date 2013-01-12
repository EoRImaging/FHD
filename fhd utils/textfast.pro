PRO TextFast,data,header,filename=filename,filepathfull=filepathfull,read=read,write=write,action=action,column_list=column_list,$
 first_line=first_line,xdr=xdr,RootDirectory=RootDirectory,string=string,append=append,extension=extension,data_directory=data_directory
;IF N_Elements(datatype) EQ 0 THEN datatype='f' ELSE IF (datatype EQ 'string') OR (datatype EQ 's') THEN datatype='a'
;dimension=(size(data,/dimension))[0]
;elements=(size(data,/dimension))[1]
;IF N_Elements(column_list) EQ 0 THEN column_list=Indgen(8)
;ncol=N_Elements(column_list)
;IF ncol GT 8 THEN BEGIN print,'no more than 8 columns of data for this program' & RETURN & ENDIF
;format=Strarr(ncol)+'x'
;format[column_list]=datatype

ON_ERROR,2

;Deal with obsolete keywords
CASE 1 OF 
    Keyword_Set(filepathfull):
    Keyword_Set(data_directory):filepathfull=data_directory
ENDCASE

IF Keyword_Set(extension) THEN filename_use=filename+extension ELSE BEGIN
    UPNAME=StrUpCase(filename)
    ptxt=strpos(UPNAME,'.TXT')
    ;IF Keyword_Set(xdr) THEN ptxt=999
    IF ptxt EQ -1 THEN filename_use=filename+String('.txt') ELSE filename_use=filename
ENDELSE
IF Keyword_Set(write) THEN action='write'
IF Keyword_Set(read) THEN action='read'
IF ~Keyword_Set(action) THEN action=' '
IF N_Elements(Rootdirectory) EQ 0 THEN Rootdirectory=rootdir()
IF N_Elements(first_line) EQ 0 THEN first_line=0
col_list_flag=(N_Elements(column_list) EQ 0) ? 0:1
;IF Keyword_Set(xdr) THEN BEGIN
; CASE action OF
;  'write':action='xdr write'
;  'read': action='xdr read'
;  ELSE:
; ENDCASE
;ENDIF
CASE action OF
    'write':BEGIN
        IF Keyword_Set(col_list_flag) THEN data_use=data[column_list,*] ELSE data_use=data
        dimension=(size(data_use,/dimension))[0]
        IF Keyword_Set(header) THEN BEGIN
            IF size(data_use,/n_dim) EQ 1 THEN elements=1 ELSE elements=(size(data_use,/dimension))[1]
            IF Keyword_Set(col_list_flag) THEN header_use=header[column_list,*] ELSE header_use=header
            head_dims=size(header_use,/dimension)
            IF N_Elements(head_dims) EQ 1 THEN head_elements=1 ELSE head_elements=(size(header_use,/dimension))[1] 
            head_dimension=(size(header_use,/dimension))[0]            
            IF head_dimension GT dimension THEN BEGIN header_use=header_use[0:dimension-1,*] & head_dimension=dimension & ENDIF
            temp_array=Strarr(dimension,elements+head_elements)
            temp_array[0:head_dimension-1,0:head_elements-1]=header_use
            temp_array[*,head_elements:*]=data_use
            data_use=temp_array
        ENDIF
        IF Keyword_Set(append) THEN BEGIN
            IF Keyword_Set(filepathfull) THEN OpenU,unit,filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),/Get_LUN,/append $
            ELSE OpenU,unit,filepath(filename_use,Root_dir=Rootdirectory),/Get_LUN,/append
        ENDIF ELSE BEGIN
            IF Keyword_Set(filepathfull) THEN OpenW,unit,filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),/Get_LUN $
            ELSE OpenW,unit,filepath(filename_use,Root_dir=Rootdirectory),/Get_LUN
        ENDELSE
        format_code=String(format='("(",I,"(A,TR1))")',dimension)
        PrintF,unit,format=format_code,data_use
        ;  WriteU,unit,data_use
        Free_LUN,unit
    END
    'read':BEGIN
        IF Keyword_Set(filepathfull) THEN Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),data_start=first_line) $
            ELSE Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory),data_start=first_line)
        IF Keyword_Set(string) THEN BEGIN
            n_columns=(size(data.(0),/dimension))[0]
            template={version:1,datastart:first_line,delimiter:byte(32),missingvalue:!Values.F_NAN,Commentsymbol:'',fieldcount:n_columns,$
            fieldtypes:lonarr(n_columns)+7,fieldlocations:indgen(n_columns),fieldnames:string(indgen(n_columns)),fieldgroups:indgen(n_columns)}
            ;   template={version:1,datastart:first_line,delimiter:32,missingvalue:!Values.F_NAN,Commentsymbol:'',fieldcount:n_columns,fieldtypes:lonarr(n_columns)+7,fieldnames:indgen(n_columns)}
            IF Keyword_Set(filepathfull) THEN Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),data_start=first_line,template=template) $
                ELSE Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory),data_start=first_line,template=template)
        ENDIF ELSE BEGIN
            IF Keyword_Set(filepathfull) THEN Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),data_start=first_line) $
                ELSE Data=Read_Ascii(filepath(filename_use,Root_dir=Rootdirectory),data_start=first_line)
        ENDELSE
        IF N_tags(data) GT 1 THEN BEGIN
            length=N_Elements(data.(0))
            IF Keyword_Set(string) THEN temp=Strarr(n_columns,length) ELSE temp=Fltarr(n_columns,length)
            FOR i=0,n_tags(data)-1 DO temp[i,*]=data.(i)
            data=temp
        ENDIF ELSE  data=data.(0)

        ;  IF Keyword_Set(filepathfull) THEN OpenR,unit,filepath(filename_use,Root_dir=Rootdirectory,subdir=filepathfull),/Get_LUN $
        ;   ELSE OpenR,unit,filepath(filename_use,Root_dir=Rootdirectory),/Get_LUN
        ;  ReadF,unit,data
        ;  Close,unit
        IF Keyword_Set(col_list_flag) THEN data=Reform(data[column_list,*])
        RETURN
    END
    ; 'xdr write': BEGIN
    ;  print,'option "xdr write" is not yet supported'
    ;  RETURN
    ; END
    ; 'xdr read': BEGIN
    ;  openr,lun,filepath(filename_use,Root_dir=RootDirectory,subdir=filepathfull),/get_lun
    ;  readu,lun,data
    ;  close,lun
    ; END
    ELSE:print,'please specify whether to read or write'
ENDCASE
END