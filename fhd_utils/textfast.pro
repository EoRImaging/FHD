PRO TextFast,data,header,file_path=file_path,read=read,write=write,action=action,column_list=column_list,$
 first_line=first_line,xdr=xdr,string=string,append=append,extension=extension

ON_ERROR,2

UPNAME=StrUpCase(file_path)
ptxt=strpos(UPNAME,'.TXT')
IF ptxt EQ -1 THEN file_path_use=file_path+'.txt' ELSE file_path_use=file_path
IF Keyword_Set(write) THEN action='write'
IF Keyword_Set(read) THEN action='read'
IF ~Keyword_Set(action) THEN action=' '
IF N_Elements(first_line) EQ 0 THEN first_line=0
col_list_flag=(N_Elements(column_list) EQ 0) ? 0:1

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
        IF Keyword_Set(append) THEN OpenU,unit,file_path_use,/Get_LUN,/append $
            ELSE OpenW,unit,file_path_use,/Get_LUN
        format_code=String(format='("(",I,"(A,TR1))")',dimension)
        PrintF,unit,format=format_code,data_use
        Free_LUN,unit
    END
    'read':BEGIN
        IF Keyword_Set(string) THEN BEGIN
            Data=Read_Ascii(file_path_use,data_start=first_line)
            n_columns=(size(data.(0),/dimension))[0]
            template={version:1,datastart:first_line,delimiter:byte(32),missingvalue:!Values.F_NAN,Commentsymbol:'',fieldcount:n_columns,$
            fieldtypes:lonarr(n_columns)+7,fieldlocations:indgen(n_columns),fieldnames:string(indgen(n_columns)),fieldgroups:indgen(n_columns)}
            Data=Read_Ascii(file_path_use,data_start=first_line,template=template)
        ENDIF ELSE BEGIN
            Data=Read_Ascii(file_path_use,data_start=first_line)
        ENDELSE
        IF N_tags(data) GT 1 THEN BEGIN
            length=N_Elements(data.(0))
            IF Keyword_Set(string) THEN temp=Strarr(n_columns,length) ELSE temp=Fltarr(n_columns,length)
            FOR i=0,n_tags(data)-1 DO temp[i,*]=data.(i)
            data=temp
        ENDIF ELSE  data=data.(0)
        IF Keyword_Set(col_list_flag) THEN data=Reform(data[column_list,*])
    END
    ELSE:print,'please specify whether to read or write'
ENDCASE
END