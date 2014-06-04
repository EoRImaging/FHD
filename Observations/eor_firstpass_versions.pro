pro eor_firstpass_versions
except=!except
!except=0
heap_gc

; wrapper to contain all the parameters for various runs we might do
; using firstpass.

; parse command line args
compile_opt strictarr
args = Command_Line_Args(count=nargs)
obs_id = args[0]
output_directory = args[1]
version = args[2]
cmd_args={version:version}

; Set default values for everything
calibrate_visibilities=1
recalculate_all=0
export_image=1
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
save_visibilities=1
calibration_visibilities_subtract=0
return_cal_visibilities=1
snapshot_healpix_export=1
n_avg=2
ps_kbinsize=0.5
ps_kspan=600.
image_filter_fn='filter_uv_uniform'

uvfits_version=2
uvfits_subversion=0

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=2048
max_sources=20000
pad_uv_image=2.
FoV=0
no_ps=1
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
nfreq_avg=16
no_rephase=1
combine_obs=0
smooth_width=11.
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1
cal_cable_reflection_fit=150
restrict_hpx_inds=1

kbinsize=0.5
psf_resolution=100

case version of
   'apb_test_restrict_hpx_inds_1': begin
      restrict_hpx_inds='EoR0_high_healpix_inds.idlsave'       ; now graduated to a default
   end
   'apb_test_galaxy_cal_1': begin
      galaxy_calibrate=1
      image_filter_fn='filter_uv_natural'
   end
   'apb_test_pattis_catalog_1': begin
      calibration_catalog_file_path=filepath('pattis_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
   'apb_test_pattis_catalog_2': begin
      calibration_catalog_file_path=filepath('pattis_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
      flag_visibilities=0
   end
   'apb_test_2s_2': begin
      uvfits_subversion=3
      restrict_hpx_inds='EoR1_high_healpix_inds.idlsave'
   end
   'apb_make_EoR1_hpx_list': begin
      undefine,restrict_hpx_inds
   end
   'nb_firstpass_cablefit_no90': begin
      force_no_data=1
      allow_sidelobe_cal_sources=0  ;made before subtracting sidelobes
      ;vis_cal_polyfit.pro: line 86 changed to cable_cut_i=where(cable_len EQ 90,n_cable_cut)
   end
   'apb_test_small_kpix_1': begin
      ps_kbinsize=1.5
   end
   'apb_test_small_kpix_2': begin
      ps_kbinsize=0.75
   end
   'apb_test_small_kpix_3': begin
      ps_kbinsize=0.5
   end
   'apb_test_small_kpix_4': begin
      FoV=0
      kbinsize=0.5
      ps_kbinsize=0.5
   end
   'apb_test_dig_gain_fix_1': begin
      uvfits_version=3
      uvfits_subversion=1
      snapshot_healpix_export=0
   end
   'apb_test_no_flagging_1': begin
      flag_visibilities=0
   end
   'apb_test_no_flagging_2': begin
      flag_visibilities=0
      no_frequency_flagging=1
   end
   'apb_test_psf_resolution_1': begin
     psf_resolution=100.
   end
   'nb_firstpass_memo': begin
      uvfits_version=3
      uvfits_subversion=0
   end
   'apb_test_no_flagging_3': begin
      unflag_all=1
      flag_visibilities=0
      flag_calibration=0
   end
   'apb_test_no_flagging_4': begin
      flag_visibilities=0
      no_frequency_flagging=1
      nfreq_avg=1
   end
   'nb_test_old_cotter_1': begin
      n_avg=1
   end

   ;;; Versions below this are with new defaults as of 23 May, 2014 !!!
   
   'nb_test_new_cotter_1': begin
      vis_freq_average=2
      uvfits_version=3
      uvfits_subversion=1
      time_cut=[2,-2]
   end
   'apb_test_pattis_catalog_1_0': begin
      calibration_catalog_file_path=filepath('pattis_catalog_1_0.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_pattis_catalog_1_1': begin
      calibration_catalog_file_path=filepath('pattis_catalog_1_1.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_pattis_catalog_2_0': begin
      calibration_catalog_file_path=filepath('pattis_catalog_2_0.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_devel_merge_1': begin
      psf_resolution=32
   end
   else: print,'Default parameters'
endcase
   
SPAWN, 'python /nfs/grs1915/ha/nbarry/scripts/read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
  STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
undefine,uvfits_version ; don't need these passed further
undefine,uvfits_subversion
undefine,obs_id

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

extra=var_bundle() ; bundle all the variables into a structure

general_obs,_Extra=extra

end
