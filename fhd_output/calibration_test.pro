PRO calibration_test,fhd_file_list,output_path=output_path
except=!except
!except=0 
heap_gc

;IF N_Elements(version) EQ 0 THEN version=0
;IF N_Elements(data_directory) EQ 0 THEN data_directory=filepath('',root='DATA',subdir=['X16','Drift'])

combine_obs_sources,fhd_file_list,calibration,source_list,/restore_last,output_path=output_path

cal_use=calibration
fi_use=where(cal_use,n_use,complement=fi_cut,ncomp=n_cut)
cal_use[fi_use]=1./cal_use[fi_use]

n_files=N_Elements(fhd_file_list)

calibration_file_header=['filename','degpix','obsra',' obsdec','zenra','zendec','calibration',$
    'peak_beam_XX','peak_beam_YY','normalization','frac_uv_cover']
textfast,calibration_file_header,file_path=output_path+'_calibration',/write

FOR fi0=0,n_use-1 DO BEGIN
    fi=fi_use[fi0]
    file_path=fhd_file_list[fi]
    restore,file_path+'_fhd.sav'
;   save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
;       beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
    restore,file_path+'_output.sav'
;    save,mrc_cat,mrc_image,beam_mask,beam_avg,instr_images,stokes_images,instr_sources,stokes_sources,$
;        beam_est,model_uv_arr,model_holo_arr,calibration,p_map_simple,p_corr_simple,filename=file_path+'_output.sav'
    restore,file_path+'_obs.sav'
    
    source_uv_mask=fltarr(obs.dimension,obs.elements)
    FOR pol_i=0,1 DO source_uv_mask[where(*weights_arr[pol_i])]=1.
    uv_use_frac=Mean(source_uv_mask)
    
    cx=obs.obsx
    cy=obs.obsy
    temp_out=Strarr(11)
    temp_out[0]=file_basename(file_path,'_cal.uvfits',/fold_case)
    temp_out[1:*]=[obs.degpix,obs.obsra,obs.obsdec,obs.zenra,obs.zendec,cal_use[fi],$
        (*beam_base[0])[cx,cy],(*beam_base[1])[cx,cy],normalization,uv_use_frac] 
    textfast,temp_out,file_path=output_path+'_calibration',/append,/write
    heap_gc
ENDFOR

END