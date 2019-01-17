PRO code_reference,set_iter=set_iter,recalculate_all=recalculate_all,use_hash=use_hash,reference_hash=reference_hash,iter_ref=iter_ref,skip_ps=skip_ps,_Extra=extra
except=!except
;NOTE: to go back to an earlier commit HASH123 for testing, use git reset --hard HASH123 
;NOTE: requires the Power Spectrum (PS) repository to work (https://github.com/miguelfmorales/PS)
!except=0 
heap_gc

git,'rev-parse --abbrev-ref HEAD',repo=rootdir('fhd'),result=branch & branch=branch[0]
git,'describe',result=version,repo_path=rootdir('FHD') & version=version[0]

version=version+'_'+branch
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'version') THEN version=extra.version
data_directory=rootdir('mwa')+filepath('',root='DATA3',subdir=['128T','code_reference'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
IF n_files EQ 0 THEN vis_file_list=file_search(data_directory,'*.uvfits.sav',count=n_files) ;compatibility with my laptop 

IF N_Elements(set_iter) GT 0 THEN iter=set_iter
IF Keyword_Set(use_hash) THEN BEGIN
    dir_list=file_search(data_directory,'fhd_*'+path_sep(),/Test_directory,count=n_directories)
    
    hash_match=Strpos(file_basename(dir_list),use_hash)
    false_match=Strpos(file_basename(dir_list),'minus')
    match_i=where((hash_match GE 0) AND (false_match EQ -1),n_match)
    CASE n_match OF
        0: BEGIN print,"Hash"+use_hash+" not found! Returning." & RETURN & END
        1: version_use=file_basename(dir_list[match_i])
        ELSE: IF N_Elements(iter) GT 0 THEN version_use=file_basename(dir_list[match_i[iter<(n_match-1)]]) $
            ELSE version_use=file_basename(dir_list[Max(match_i)])
    ENDCASE
    IF N_Elements(iter) EQ 0 THEN iter=0
    IF Strpos(version_use,'fhd_') EQ 0 THEN version_use=strmid(version_use,4)
ENDIF ELSE version_use=version+(Keyword_Set(iter) ? '_run'+Strn(iter):'')

IF N_Elements(iter) EQ 0 THEN BEGIN
    test_dir=filepath('',root=data_directory,sub='fhd_'+version_use)
    iter=0
    WHILE file_test(test_dir,/directory) EQ 1 DO BEGIN
        iter+=1
        IF iter GE 100 THEN BEGIN
            print,'Could not make fhd directory, too many versions exist: fhd_'+version
            RETURN
        ENDIF
        version_use=version+'_run'+Strn(iter)
        test_dir=filepath('',root=data_directory,sub='fhd_'+version_use)
    ENDWHILE
ENDIF
fhd_file_list=fhd_path_setup(vis_file_list,version=version_use,_Extra=extra)
undefine_fhd,branch,di,match_i,n_match,hash_match,dir_list,test_dir

healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version_use,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('master_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

firstpass=1

dft_threshold=0
double=1
calibrate_visibilities=1
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF Keyword_Set(recalculate_all) THEN export_image=1 ELSE export_images=0
cleanup=0
ps_export=0
split_ps_export=1
combine_healpix=0
deconvolve=0
mapfn_recalculate=0
healpix_recalculate=0
flag_visibilities=0
vis_baseline_hist=1
silent=0
save_visibilities=0
calibration_visibilities_subtract=0
return_cal_visibilities=1
snapshot_healpix_export=1
n_avg=2
ps_kbinsize=0.5
ps_kspan=600.
image_filter_fn='filter_uv_uniform'

dimension=2048
max_sources=20000
pad_uv_image=1.
FoV=0
no_ps=1
min_baseline=1.
min_cal_baseline=50.
;ring_radius=10.*pad_uv_image
nfreq_avg=4 ;currently 16 in firstpass
no_rephase=1
combine_obs=0
smooth_width=11.
bandpass_calibrate=1
calibration_polyfit=1
cal_amp_degree_fit=2
cal_phase_degree_fit=1
no_restrict_cal_sources=1
cal_mode_fit=1
cal_reflection_mode_file=150
restrict_hpx_inds=1
output_residual_histogram=1
show_beam_contour=1
contour_level=[0,0.01,0.05,0.1,0.2,0.5,0.67,0.9]
contour_color='blue'

kbinsize=0.5
psf_resolution=32

; some new defaults (possibly temporary)
beam_model_version=2
dipole_mutual_coupling_factor=1
calibration_flag_iterate = 0
no_calibration_frequency_flagging=1
log_store=1 ;store output log to a file 
time_cut=[-2,2]

;defaults not set in firstpass:
interpolate_kernel=1
mark_zenith=1
beam_diff_image=1
beam_residual_threshold=0.1
no_fits=1
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
n_pol=2
max_cal_iter=100.
restore_vis_savefile=1
export_images=1
plot_k0_power=1
default_diffuse='D:\MWA\IDL_code\FHD\catalog_data\diffuse_pol_test.sav'
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'diffuse_calibrate') THEN IF extra.diffuse_calibrate EQ 1 THEN $
    extra=structure_update(extra,diffuse_calibrate=default_diffuse)
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'diffuse_model') THEN IF extra.diffuse_model EQ 1 THEN BEGIN
    extra=structure_update(extra,diffuse_model=default_diffuse)
    IF ~(Tag_exist(extra,'model_visibilities') OR (N_Elements(model_visibilities) GT 0)) THEN model_visibilities=1
ENDIF
IF N_Elements(extra) GT 0 THEN cmd_args=extra
extra=var_bundle()
fhd_depreciation_test,_extra=extra
IF Tag_exist(extra,'comment') THEN BEGIN
    log_file=filepath('code_reference_log.txt',root=data_directory)
    append=0
    IF file_test(log_file) EQ 0 THEN header=['Commit','Date','Comment'] ELSE append=1
    log_comment=[version_use,Systime(),extra.comment]
    Textfast,log_comment,header,/write,append=append,file_path=log_file
ENDIF
general_obs,_Extra=extra

IF ~Keyword_Set(skip_ps) THEN ps_wrapper,file_dirname(fhd_file_list[0]),/png

IF Keyword_Set(reference_hash) THEN BEGIN
    hash_diff=Strmid(version_use,Strpos(version_use,'-g')+2,7)
    code_reference_diff,hash_ref=reference_hash,hash_diff=hash_diff,iter_ref=iter_ref,iter_diff=iter,_Extra=cmd_args
ENDIF

!except=except
END
