PRO fhd_log_settings,file_path_fhd,obs=obs,antenna=antenna,psf=psf,cal=cal,fhd=fhd,cmd_args=cmd_args,overwrite=overwrite

descr_file_path=file_path_fhd+'_settings.txt'

delimiter=String(9B)
main_delimiter='##MAIN'
cmd_delimiter='##COMMAND_LINE'
obs_delimiter='##OBS'
antenna_delimiter='##ANTENNA'
psf_delimiter='##PSF'
cal_delimiter='##CAL'
fhd_delimiter='##FHD'
end_delimiter='##END'
filler='##'
max_len=132

main_insert=Strarr(2,4) +filler
main_insert[1,0]+=delimiter
main_insert[0,*]=[main_delimiter,'Calling program','Machine','User']
call_name=scope_traceback()
IF N_Elements(call_name) GT 1 THEN call_name=call_name[1] ELSE call_name=call_name[0]
main_insert[1,1]=(Strsplit(call_name,'<',/extract))[0]
login=get_login_info()
machine=StrCompress(login.machine_name,/remove_all)
user=StrCompress(login.user_name,/remove_all)
main_insert[1,2]=machine
main_insert[1,3]=user

info_out=main_insert
;info_out=[[main_delimiter,filler],[main_insert,filler],[obs_delimiter,filler],[psf_delimiter,filler],$
;    [cal_delimiter,filler],[fhd_delimiter,filler],[end_delimiter,filler]]

overwrite_test=file_test(descr_file_path)
IF Keyword_Set(overwrite) THEN overwrite_test=0
IF overwrite_test THEN BEGIN
    textfast,info_arr,/read,file_path=descr_file_path,/string,delimiter=delimiter
    info_labels=Reform(info_arr[0,*])
    cmd_i=where(info_labels EQ cmd_delimiter)
    obs_i=where(info_labels EQ obs_delimiter)
    antenna_i=where(info_labels EQ antenna_delimiter)
    psf_i=where(info_labels EQ psf_delimiter)
    cal_i=where(info_labels EQ cal_delimiter)
    fhd_i=where(info_labels EQ fhd_delimiter)
    end_i=where(info_labels EQ end_delimiter)
    
    IF Min([cmd_i,obs_i,antenna_i,psf_i,cal_i,fhd_i,end_i]) GE 0 THEN BEGIN
        cmd_insert=info_arr[0:1,cmd_i:obs_i-1]
        obs_insert=info_arr[0:1,obs_i:antenna_i-1]
        antenna_insert=info_arr[0:1,antenna_i:psf_i-1]
        psf_insert=info_arr[0:1,psf_i:cal_i-1]
        cal_insert=info_arr[0:1,cal_i:fhd_i-1]
        fhd_insert=info_arr[0:1,fhd_i:end_i-1]
    ENDIF
ENDIF    

IF Keyword_Set(cmd_args) THEN cmd_insert=structure_to_text(cmd_args,head=cmd_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(obs) THEN obs_insert=structure_to_text(obs,head=obs_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(antenna) THEN antenna_insert=structure_to_text(antenna,head=antenna_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(psf) THEN psf_insert=structure_to_text(psf,head=psf_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(cal) THEN cal_insert=structure_to_text(cal,head=cal_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(fhd) THEN fhd_insert=structure_to_text(fhd,head=fhd_delimiter,delimiter=delimiter,max_len=max_len)

IF N_Elements(cmd_insert) EQ 0 THEN cmd_insert=[cmd_delimiter,filler]
IF N_Elements(obs_insert) EQ 0 THEN obs_insert=[obs_delimiter,filler]
IF N_Elements(antenna_insert) EQ 0 THEN antenna_insert=[antenna_delimiter,filler]
IF N_Elements(psf_insert) EQ 0 THEN psf_insert=[psf_delimiter,filler]
IF N_Elements(cal_insert) EQ 0 THEN cal_insert=[cal_delimiter,filler]
IF N_Elements(fhd_insert) EQ 0 THEN fhd_insert=[fhd_delimiter,filler]

info_out=[[info_out],[cmd_insert],[obs_insert],[antenna_insert],[psf_insert],[cal_insert],[fhd_insert],[end_delimiter,filler]]
empty_test=where(info_out EQ '',n_empty)
IF n_empty GT 0 THEN info_out[empty_test]=filler
textfast,info_out,/write,file_path=descr_file_path,delimiter=delimiter

END