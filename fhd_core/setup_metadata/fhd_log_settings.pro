FUNCTION extract_settings,info_arr,delimiter=delimiter, filler=filler
info_labels=Reform(info_arr[0,*])
start_i=where(info_labels EQ delimeter, ind_present)
IF ~ind_present THEN RETURN,[delimiter, filler]
ind_list = StrMatch(info_labels[start_i+1], filler)
end_i = (where(ind_list))[0]

insert = info_arr[*, start_i:end_i-1]

RETURN, insert
END


PRO fhd_log_settings,file_path_fhd, obs=obs, layout=layout, antenna=antenna, psf=psf, cal=cal, skymodel=skymodel,$
    fhd=fhd, cmd_args=cmd_args, overwrite=overwrite, sub_directory=sub_directory

descr_file_path=file_path_fhd+'_settings.txt'
IF Keyword_Set(sub_directory) THEN descr_file_path=filepath(file_basename(descr_file_path),root=file_dirname(descr_file_path),sub=sub_directory)
IF file_test(file_dirname(descr_file_path)) EQ 0 THEN file_mkdir,file_dirname(descr_file_path)

delimiter=String(9B)
main_delimiter='##MAIN'
cmd_delimiter='##COMMAND_LINE'
obs_delimiter='##OBS'
layout_delimiter='##LAYOUT'
antenna_delimiter='##ANTENNA'
psf_delimiter='##PSF'
cal_delimiter='##CAL'
skymodel_delimiter='##SKYMODEL'
fhd_delimiter='##FHD'
end_delimiter='##END'
filler='##'
max_len=132

main_insert=Strarr(2,5) +filler
main_insert[1,0]+=delimiter
main_insert[0,*]=[main_delimiter,'Calling program','Machine','User','Date']
call_name=scope_traceback()
IF N_Elements(call_name) GT 1 THEN call_name=call_name[1] ELSE call_name=call_name[0]
main_insert[1,1]=(Strsplit(call_name,'<',/extract))[0]
login=get_login_info()
machine=StrCompress(login.machine_name,/remove_all)
user=StrCompress(login.user_name,/remove_all)
date=Systime()
main_insert[1,2]=machine
main_insert[1,3]=user
main_insert[1,4]=date

overwrite_test=file_test(descr_file_path)
IF Keyword_Set(overwrite) THEN overwrite_test=0
IF overwrite_test THEN BEGIN
    textfast,info_arr,/read,file_path=descr_file_path,/string,delimiter=delimiter
    main_insert = extract_settings(info_arr, delimiter=main_delimiter, filler=filler)
    cmd_insert = extract_settings(info_arr, delimiter=cmd_delimiter, filler=filler)
    obs_insert = extract_settings(info_arr, delimiter=obs_delimiter, filler=filler)
    layout_insert = extract_settings(info_arr, delimiter=layout_delimiter, filler=filler)
    antenna_insert = extract_settings(info_arr, delimiter=antenna_delimiter, filler=filler)
    psf_insert = extract_settings(info_arr, delimiter=psf_delimiter, filler=filler)
    cal_insert = extract_settings(info_arr, delimiter=cal_delimiter, filler=filler)
    skymodel_insert = extract_settings(info_arr, delimiter=skymodel_delimiter, filler=filler)
    fhd_insert = extract_settings(info_arr, delimiter=fhd_delimiter, filler=filler)
ENDIF    

IF Keyword_Set(cmd_args) THEN cmd_insert=structure_to_text(cmd_args,head=cmd_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(obs) THEN obs_insert=structure_to_text(obs,head=obs_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(layout) THEN layout_insert=structure_to_text(layout,head=layout_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(antenna) THEN antenna_insert=structure_to_text(antenna,head=antenna_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(psf) THEN psf_insert=structure_to_text(psf,head=psf_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(cal) THEN cal_insert=structure_to_text(cal,head=cal_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(skymodel) THEN skymodel_insert=structure_to_text(skymodel,head=skymodel_delimiter,delimiter=delimiter,max_len=max_len)
IF Keyword_Set(fhd) THEN fhd_insert=structure_to_text(fhd,head=fhd_delimiter,delimiter=delimiter,max_len=max_len)

IF N_Elements(cmd_insert) EQ 0 THEN cmd_insert=[cmd_delimiter,filler]
IF N_Elements(obs_insert) EQ 0 THEN obs_insert=[obs_delimiter,filler]
IF N_Elements(layout_insert) EQ 0 THEN layout_insert=[layout_delimiter,filler]
IF N_Elements(antenna_insert) EQ 0 THEN antenna_insert=[antenna_delimiter,filler]
IF N_Elements(psf_insert) EQ 0 THEN psf_insert=[psf_delimiter,filler]
IF N_Elements(cal_insert) EQ 0 THEN cal_insert=[cal_delimiter,filler]
IF N_Elements(skymodel_insert) EQ 0 THEN skymodel_insert=[skymodel_delimiter,filler]
IF N_Elements(fhd_insert) EQ 0 THEN fhd_insert=[fhd_delimiter,filler]

info_out=[[main_insert],[cmd_insert],[obs_insert],[layout_insert],[antenna_insert],[psf_insert],[cal_insert],[skymodel_insert],[fhd_insert],[end_delimiter,filler]]
empty_test=where(info_out EQ '',n_empty)
IF n_empty GT 0 THEN info_out[empty_test]=filler
textfast,info_out,/write,file_path=descr_file_path,delimiter=delimiter

END
