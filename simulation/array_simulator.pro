PRO array_simulator,vis_arr,flag_arr,obs,status_str,psf,params,jones,error=error,$
    instrument=instrument,file_path_vis,cleanup=cleanup,recalculate_all=recalculate_all,$
    n_pol=n_pol,silent=silent,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,freq_start=freq_start,freq_end=freq_end,$
    eor_sim=eor_sim,include_catalog_sources = include_catalog_sources, source_list=source_list,$
    catalog_file_path=catalog_file_path,_Extra=extra
    
compile_opt idl2,strictarrsubs
except=!except
!except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
error=0
heap_gc
t0=Systime(1)
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
IF N_Elements(silent) EQ 0 THEN silent=0
  
fhd_save_io,status_str,file_path_fhd=file_path_fhd
  
print,'Simulating: ',file_path_vis
print,systime()
print,'Output file_path:',file_path_fhd

IF Keyword_Set(n_pol) THEN vis_test=Min(status_str.vis_ptr[0:n_pol-1]) ELSE vis_test=status_str.vis_ptr[0]
metadata_test=status_str.obs<status_str.params<status_str.psf<status_str.jones
IF Keyword_Set(recalculate_all) THEN vis_test=(metadata_test=0)

IF vis_test AND metadata_test THEN BEGIN
    fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    n_pol=obs.n_pol
    vis_arr=Ptrarr(n_pol)
    IF ~silent THEN print,"Restoring saved visibilities (this may take a while)"
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,vis_ptr,var='vis_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        vis_arr[pol_i]=vis_ptr
    ENDFOR
    IF ~silent THEN print,"...Done"
    RETURN
ENDIF

;set up the obs and params structure. Will generate arbitary array layout if supplied
array_simulator_init,obs,params,error=error,instrument=instrument,_Extra=extra
n_pol=obs.n_pol
n_freq=obs.n_freq

;Read in or construct a new beam model. Also sets up the structure PSF
IF ~silent THEN print,'Calculating beam model'
psf=beam_setup(obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=0,_Extra=extra)
IF Keyword_Set(t_beam) THEN IF ~silent THEN print,'Beam modeling time: ',t_beam

flag_arr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO *flag_arr[pol_i]=Replicate(1.,n_freq,N_Elements(params.uu))
flag_arr=vis_flag_basic(flag_arr,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
    freq_end=freq_end,tile_flag_list=tile_flag_list,instrument=instrument,_Extra=extra)
vis_flag_update,flag_arr,obs,psf,params
;print informational messages
IF ~silent THEN obs_status,obs


;Construct model visibilities. Start by building a model u-v-f cube


END