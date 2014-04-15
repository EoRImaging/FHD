pro eor_firstpass_versions
except=!except
!except=0
heap_gc

; wrapper to contain all the parameters for various runs we might do
; using firstpass.

; parse command line args
compile_opt strictarr
args = Command_Line_Args(count=nargs)
ods_id = args[0]
output_directory = args[1]
version = args[2]

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
flag_visibilities=1
vis_baseline_hist=1
silent=0
save_visibilities=1
calibration_visibilities_subtract=0
return_cal_visibilities=1
snapshot_healpix_export=1
n_avg=2
ps_kbinsize=3.
ps_kspan=600.
image_filter_fn='filter_uv_uniform'

uvfits_version=2
uvfits_subversion=0
SPAWN, 'python /nfs/grs1915/ha/nbarry/scripts/read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
  STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
undefine,uvfits_version ; don't need these passed further
undefine,uvfits_subversion
undefine,obs_id

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=2048
max_sources=20000
pad_uv_image=2.
FoV=80.
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

case version of
   'apb_test_restrict_hpx_inds_1': begin
      print,'using parameters for version '+version
      restrict_hpx_inds=1
   end
   'apb_test_galaxy_cal_1': begin
      print,'using parameters for version '+version
      galaxy_calibrate=1
   end
   else: print,'Default parameters'
endcase

extra=var_bundle() ; bundle all the variables into a structure

general_obs,_Extra=extra

end
