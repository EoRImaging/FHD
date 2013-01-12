;+
; :Description:
;    Sets up the base file_path to use for all data on either Windows or Mac computers.
;    Sets defaults if parameters are not supplied.
;
; :Params:
;    data_directory - working directory
;    
;    filename - uvfits filename, omitting the .uvfits extension. If the data is already calibrated, it should end with .cal.uvfits instead of just .uvfits
;    
;    file_path - full file path to 'filename'
;
; :Keywords:
;    obs - structure containing details of the observation
;
; :Author: isullivan 2012
;-
PRO vis_path_default,data_directory,filename,file_path,obs=obs,version=version,old_style=old_style,_Extra=extra

IF Keyword_Set(obs) THEN BEGIN
    data_directory=obs.data_directory
    filename=obs.filename
    version=obs.version
;    file_path=obs.path
;    pos=Strpos(file_path,'\',/reverse_search)
;    data_directory=Strmid(file_path,0,pos)
;    filename=Strmid(file_path,pos+1)
;    root_dir=rootdir('mwa')
;    pos2=Strpos(data_directory,root_dir,/reverse_search)
;    IF pos2 NE -1 THEN data_directory=Strmid(data_directory,strlen(root_dir))
ENDIF
os_type=!version.os_family
CASE os_type OF
    'Windows':BEGIN
        IF not Keyword_Set(filename) THEN BEGIN
;            filename='PicA_121_20100924211938_cal'
            filename='P00_drift_121_20110927130001_cal'
        ENDIF
        IF not Keyword_Set(data_directory) THEN BEGIN
;            data_directory='DATA\r4\clmw\X14\PicA_121_20100924211938'
;            UPNAME=StrUpCase(filename)
;            pcal=strpos(UPNAME,'_CAL')
;            IF pcal GT 0 THEN filename_use=StrMid(filename,0,pcal) ELSE filename_use=filename
            data_directory=filepath('',root='DATA\X16\Drift');,sub=filename_use)            
        ENDIF
    END
    'unix': BEGIN
        IF not Keyword_Set(filename) THEN BEGIN
            filename='P00_drift_121_20110927130001_cal'
        ENDIF
        IF not Keyword_Set(data_directory) THEN BEGIN
            UPNAME=StrUpCase(filename)
            pcal=strpos(UPNAME,'_CAL')
            IF pcal GT 0 THEN filename_use=StrMid(filename,0,pcal) ELSE filename_use=filename
            data_directory=filepath('',root='DATA/X16/Drift');,sub=filename_use)
        ENDIF
    END
ENDCASE

IF N_Elements(version) EQ 0 THEN version=0 ELSE version=Fix(version)
version_name='v'+strn(version)
version_dirname='fhd_'+version_name
IF Keyword_Set(old_style) THEN dir=filepath('',root=data_directory,sub=[filename,version_dirname]) $
    ELSE dir=filepath('',root=data_directory,sub=[version_dirname,filename])
IF file_test(rootdir('mwa')+dir) EQ 0 THEN file_mkdir,rootdir('mwa')+dir   
file_path=filepath(filename+version_name,root_dir=rootdir('mwa')+dir)
END