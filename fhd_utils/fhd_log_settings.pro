PRO fhd_log_settings,file_path_fhd,obs=obs,psf=psf,cal=cal,fhd=fhd

descr_file_path=file_path_fhd+'_settings.txt'

main_delimeter='##MAIN'
obs_delimeter='##OBS'
psf_delimeter='##PSF'
cal_delimeter='##CAL'
fhd_delimeter='##FHD'
end_delimeter='##END'

main_insert=scope_traceback()
IF N_Elements(main_insert) GT 1 THEN main_insert=main_insert[1] ELSE main_insert=main_insert[0]
info_out=[[main_delimeter,''],[main_insert,'']]
;info_out=[[main_delimeter,''],[main_insert,''],[obs_delimeter,''],[psf_delimeter,''],$
;    [cal_delimeter,''],[fhd_delimeter,''],[end_delimeter,'']]

IF file_test(descr_file_path) THEN BEGIN
    textfast,info_arr,/read,file_path=descr_file_path 
    info_labels=Reform(info_out[0,*])
    obs_i=where(info_labels EQ obs_delimeter)
    psf_i=where(info_labels EQ psf_delimeter)
    cal_i=where(info_labels EQ cal_delimeter)
    fhd_i=where(info_labels EQ fhd_delimeter)
    end_i=where(info_labels EQ end_delimeter)
    
    IF Min([obs_i,psf_i,cal_i,fhd_i,end_i]) GE 0 THEN BEGIN
        obs_insert=info_arr[*,obs_i:psf_i-1]
        psf_insert=info_arr[*,psf_i:cal_i-1]
        cal_insert=info_arr[*,cal_i:fhd_i-1]
        fhd_insert=info_arr[*,fhd_i:end_i-1]
    ENDIF
ENDIF    

IF Keyword_Set(obs) THEN obs_insert=structure_to_text(obs,head=obs_delimeter)
IF Keyword_Set(psf) THEN psf_insert=structure_to_text(psf,head=psf_delimeter)
IF Keyword_Set(cal) THEN cal_insert=structure_to_text(cal,head=cal_delimeter)
IF Keyword_Set(fhd) THEN fhd_insert=structure_to_text(fhd,head=fhd_delimeter)

IF N_Elements(obs_insert) EQ 0 THEN obs_insert=[obs_delimeter,'']
IF N_Elements(psf_insert) EQ 0 THEN psf_insert=[psf_delimeter,'']
IF N_Elements(cal_insert) EQ 0 THEN cal_insert=[cal_delimeter,'']
IF N_Elements(fhd_insert) EQ 0 THEN fhd_insert=[fhd_delimeter,'']

info_out=[[info_out],[obs_insert],[psf_insert],[cal_insert],[fhd_insert],[end_delimeter,'']]
textfast,info_out,/write,file_path=descr_file_path 

END