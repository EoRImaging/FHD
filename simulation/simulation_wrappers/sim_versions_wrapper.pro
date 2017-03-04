PRO sim_versions_wrapper, version=version, sources_file_name=sources_file_name, catalog_file_name=catalog_file_name,fov=fov,$
    set_sidelobe_keywords=set_sidelobe_keywords;, _Extra=extra

except=!except
!except=0
heap_gc 


; parse command line args
compile_opt strictarr
args = Command_Line_Args(count=nargs)
obs_id = args[0]
output_directory = args[1]
version = args[2]

if nargs eq 4 then begin
	; This will be used if being run in variation mode. For this, I'll need to set an obs_id_suffix
	print, "Passed successfully"
	param_set=args[3]
endif

cmd_args={version:version}

if n_elements(obs_id) eq 0 then begin
	print, n_elements(obs_id) eq 0
	print, 'Bad'
endif

uvfits_version=4
uvfits_subversion=1


;Set defaults
;sources_file_name="zem_simulation_sources/sim_source_list1"  ; Temporarily using smaller source list to improve gridding time.
sources_file_name="mwa_calibration_source_list_gleam_kgs_fhd_fornax"
;sources_file_name="point_source2_paper" ; move the source to RA of 0.
export_images = 1 ; toggles the output of images from fhd_quickview
save_visibilities= 1
model_visibilities=1
make_regular_bls=0
save_antenna_model=1
snapshot_healpix_export = 1
ps_export=1
split_ps_export=1
save_imagecube=0
save_uvf=0
dft_threshold=0   ; Approximate the DFT (1) or not (0)

restore_last=1   ;    Use saved beam model

beam_offset_time=0   ; How far into the file to calculate the beam model? (seconds)

;grid_interpolate=1   ; This doesn't see to do anything..
unflag_all=1

n_pol = 2
image_filter_fn='filter_uv_uniform' ; not sure if this makes sense for simulations
vis_baseline_hist=1
;dimension=1024
dimension=2048
if n_elements(fov) eq 0 then fov=0.   ; Smaller for HERA, maybe 10 to 15. (80 originally) -- overrides kbinsize
nfreq_avg=10    ; Fine frequencies per coarse channel.

max_sources=10000
psf_resolution=100.   ;   Ask Ian -- does this set subgrid resolution, and is interpolation on? (interpolate instead of subgrid)
kbinsize=0.5
;kbinsize=0.25
;recalculate_all=0 ; If there is no data being used for context, FHD should calculate everything from scratch.
grid_recalculate = 1 ; This is needed in array_simulator to enter the gridding/imaging sections
include_catalog_sources = 1 ; toggles the use of catalog sources in the simulation source list.
eor_sim = 0; toggles eor simulation in vis_simulate

no_rephase=1 ;set to use obsra, obsdec for phase center even if phasera, phasedec present in a .metafits file
;force_rephase_to_zenith=1
; To work with Bryna's changes for noise simulations, include_noise must be explicitly off. Should be investigated and fixed!
include_noise=0
noise_sigma_freq=0

;Convoluted way of setting up 'instrument' for use here, while still leaving it to be passed in Extra
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'instrument') THEN instrument=extra.instrument
IF N_Elements(instrument) EQ 0 THEN instrument='hera'
IF N_Elements(double_precison_beam) EQ 0 THEN double_precison_beam=0


if n_elements(version) eq 0 then begin ;version provides a name for the output subdirectory
    print, 'Please provide a version designator.'
    return
endif

;;** Setting up the source_array structure that contains the sources to be used in the simulation
if n_elements(sources_file_name) eq 0 then begin 
    print, 'Please supply the name of an input file with sources to simulate.'
    return
endif else print, 'Using sources: ' + sources_file_name


case version of

   'sim_ewbase_hera_nfreqavg': begin
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	beam_model_version=2
	kbinsize=0.5
   end


   'sim_ewbase_hera_kbin_v2': begin
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	beam_model_version=2
	nfreq_avg=203
   end

   'sim_ewbase_hera_kbin_v2': begin
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	beam_model_version=2
	nfreq_avg=203
   end

   'sim_ewbase_hera_kbin_v1': begin
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	beam_model_version=2
	nfreq_avg=203
   end

   'sim_mwa_fornax_mwabeams': begin
	dimension=1024
	instrument='mwa'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=16
   end

   'sim_mwa_fornax_herabeams': begin
	dimension=1024
	instrument='paper'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=16
   end


   'sim_mwa_fornax_paperbeams': begin
	dimension=1024
	instrument='paper'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=16
   end




   'sim_paper19_lowres': begin
	dimension=1024
	instrument='paper'
	max_model_sources=7000
	sources_file_name='GLEAM_EGC_catalog'
	nfreq_avg=203
	kbinsize=2.0
   end


   'sim_hera19_lidz_eor': begin
	;First platinum file, compressed frequencies
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=203
	include_catalog_sources=0
	eor_sim=1
   end

   'sim_hera19_eor': begin
	;First platinum file, compressed frequencies
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=203
	include_catalog_sources=0
	eor_sim=1
   end

   'sim_mwa_fornax_eor_herabeam': begin
	dimension=1024
	instrument='hera'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=16
	include_catalog_sources=0
	eor_sim=1
   end


   'sim_mwa_fornax_eor_paperbeam': begin
	dimension=1024
	instrument='paper'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=16
	include_catalog_sources=0
	eor_sim=1
   end

   'sim_hera19_hires': begin
	dimension=1024
	instrument='hera'
	max_model_sources=7000
	sources_file_name='GLEAM_EGC_catalog'
	nfreq_avg=203
	kbinsize=0.25
   end

   'sim_paper128': begin
	dimension=1024
	instrument='paper'
	max_model_sources=7000
	sources_file_name="GLEAM_EGC_catalog"
	nfreq_avg=203
   end

   'sim_hera37': begin
	dimension=1024
	instrument='hera'
	max_model_sources=7000
	sources_file_name="GLEAM_EGC_catalog"
	nfreq_avg=203
   end


   'sim_hera19_v2': begin
	dimension=1024
	instrument='hera'
	max_model_sources=7000
	sources_file_name='GLEAM_EGC_catalog'
	nfreq_avg=203
	beam_model_version=2
   end

   'sim_hera19': begin
	dimension=1024
	instrument='hera'
	max_model_sources=7000
	sources_file_name='GLEAM_EGC_catalog'
	nfreq_avg=203
   end

   'sim_paper19': begin
	dimension=1024
	instrument='paper'
	max_model_sources=7000
	sources_file_name='GLEAM_EGC_catalog'
	nfreq_avg=203
   end

   'sim_mwa_fornax_eor_nometa': begin
	dimension=1024
	instrument='mwa'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=16
	include_catalog_sources=0
	eor_sim=1	
   end

   'sim_mwa_fornax_eor_meta': begin
	dimension=1024
	instrument='mwa'
	sources_file_name='GLEAM_EGC_catalog'
	max_model_sources=0
	dft_threshold=0
	nfreq_avg=16
	include_catalog_sources=0
	eor_sim=1
   end

   'test': begin
	include_catalog_sources=0
	eor_sim=1
	max_model_sources=0
;	max_calibration_sources=0
;	max_sources=0
	instrument='mwa'
	dft_threshold=1
	dimension=512
	nfreq_avg=203
   end

endcase




;print, 'max_sources = '+ String(max_model_sources)
;max_model_sources=max_sources


sources_file_path = filepath(string(format='(A,".sav")',sources_file_name),root=rootdir('FHD'), subdir='catalog_data')

;restore, sources_file_path
;source_array = catalog
;undefine, catalog

SPAWN, 'locate_uvfits_oscar.py -o ' + STRING(obs_id), vis_file_list

temp_path=vis_file_list[0] ; vis_file_list needs needs to be a scalar each time it is passed to array_simulator. For now, we are only using one file.:
undefine, vis_file_list
sim_from_uvfits_filepath = temp_path ; used in array_simulator_init.pro

print, 'Reading from file: ' + sim_from_uvfits_filepath

file_path_fhd = fhd_path_setup(sim_from_uvfits_filepath,output_directory=output_directory,version=version,_Extra=extra) ;Creates output directories for each obsid
temp_path2 = file_path_fhd[0]
undefine, file_path_fhd
file_path_fhd = temp_path2


if isa(param_set, /String) then begin
	test=execute(param_set,1)
	print, 'kbinsize set to ', kbinsize
	param_set = StrJoin( StrSplit(param_set, '=', /Extract), '-')
	file_path_fhd = file_path_fhd + "_" + param_set
endif


undefine,uvfits_version ; don't need these passed further
undefine,uvfits_subversion
undefine,obs_id



if n_elements(set_sidelobe_keywords) eq 0 then set_sidelobe_keywords=0

allow_sidelobe_image_output=set_sidelobe_keywords
allow_sidelobe_sources=set_sidelobe_keywords
allow_sidelobe_model_sources=set_sidelobe_keywords
allow_sidelobe_cal_sources=set_sidelobe_keywords



IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)

if make_regular_bls eq 1 THEN BEGIN
;	uvw = hera_baselines()
	ps_kspan=600
	simulate_baselines=1
	nsample=100
	sim_uu = findgen(nsample)*ps_kspan/nsample - ps_kspan/2
	sim_vv = findgen(nsample)*ps_kspan/nsample - ps_kspan/2
	f_use = 150e6
	sim_uu = sim_uu / f_use
	sim_vv = sim_vv / f_use

	max_n_baseline = 8000
	n_time = 2*ceil(nsample/float(max_n_baseline))
	n_per_time = floor(nsample/(n_time/2.))
	if n_per_time*n_time ne nsample then begin
	  nsample = n_per_time*n_time/2.
	  sim_uu = sim_uu[0:nsample-1]
	  sim_vv = sim_vv[0:nsample-1]
	endif
	sim_uu = reform(sim_uu, n_per_time, 1, n_time/2)
	sim_vv = reform(sim_vv, n_per_time, 1, n_time/2)
	
	sim_baseline_uu = reform([[sim_uu], [sim_uu]], n_per_time*n_time)
	sim_baseline_vv = reform([[sim_vv], [sim_vv]], n_per_time*n_time)
	
	sim_baseline_time = [intarr(n_per_time), intarr(n_per_time)+1]
	if n_time gt 2 then for i=1, n_time/2-1 do sim_baseline_time = [sim_baseline_time, intarr(n_per_time)+2*i, intarr(n_per_time)+2*i+1]
endif

;; Baseline Simulation
;IF N_Elements(n_avg) EQ 0 THEN n_avg=2
;IF N_Elements(ps_kbinsize) EQ 0 THEN ps_kbinsize=0.5
;IF N_Elements(ps_kspan) EQ 0 THEN ps_kspan=600.

extra=var_bundle()

array_simulator, vis_arr, flag_arr, obs, status_str, psf, params, jones, $   
    instrument=instrument, $ 
    recalculate_all=recalculate_all, $
    grid_recalculate=grid_recalculate, $
    file_path_fhd=file_path_fhd, $
    catalog_file_path=sources_file_path, $
    sim_from_uvfits_filepath=sim_from_uvfits_filepath, $
    source_array=source_array, $
    include_catalog_sources=include_catalog_sources, $
;    model_visibilities=model_visibilities, $
    error=error, $
    unflag_all=unflag_all, $
    eor_sim=eor_sim, $
    simulate_header=simulate_header, $
    complex=complex_beam, $
    double=double_precision_beam, $
    simulate_baselines=simulate_baselines, sim_baseline_uu=sim_baseline_uu, sim_baseline_vv=sim_baseline_vv, n_time=n_time, sim_baseline_time_inds=sim_baseline_time, $
    export_images=export_images, $
    save_visibilities=save_visibilities, $
    beam_offset_time=beam_offset_time, $
    snapshot_healpix_export=snapshot_healpix_export, $
    split_ps_export=split_ps_export, $
    save_imagecube=save_imagecube, $
    save_uvf=save_uvf,$
    save_antenna_model=save_antenna_model,$
    include_noise = include_noise, noise_sigma_freq = noise_sigma_freq, $
    n_pol=n_pol,$
    dimension=dimension, $
    fov=fov, $
    image_filter_fn=image_filter_fn, $
    nfreq_avg=nfreq_avg, $
    no_rephase=no_rephase, $
    obsra=obsra,$
    no_save=no_save,$
    allow_sidelobe_image_output=allow_sidelobe_image_output, $
    allow_sidelobe_sources=allow_sidelobe_sources,$
    allow_sidelobe_model_sources=allow_sidelobe_model_sources,$
    allow_sidelobe_cal_sources=allow_sidelobe_cal_sources,$
    psf_resolution=psf_resolution,$
    kbinsize=kbinsize,$
    restore_last=restore_last,$
    max_sources=max_sources,$
    max_model_sources=max_model_sources,$
    max_calibration_sources=max_calibration_sources,$
    dft_threshold=dft_threshold,$
    diffuse_model=diffuse_model,$
    _Extra=extra
    
heap_gc

!except=except

end
