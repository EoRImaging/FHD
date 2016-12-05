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
save_antenna_model=1
restore_last=1
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

   'sim_mwa_golden': begin
	instrument='mwa'
	;reorder_visibilities=1
	;max_sources=10000
        max_model_sources=7000
	dft_threshold=1
	nfreq_avg=16
	;max_calibration_sources=10000
    end

   'sim_mwa_golden2': begin
	instrument='mwa'
	;reorder_visibilities=1
	;max_sources=10000
        max_model_sources=7000
	dft_threshold=0
	nfreq_avg=16
	;max_calibration_sources=10000
    end

   'sim_hera128': begin
	instrument='hera'
        dft_threshold=0
   end

   'sim_hera37_1src': begin
        instrument='hera'
        sources_file_name="zem_simulation_sources/sim_source_list1"
   end

   'sim_hera37_3hour': begin
	instrume='hera'
	max_model_sources=10000
	dft_threshold=1
	nfreq_avg=203
   end

   'sim_paper128_at_MRO': begin
	instrument='paper'
	dft_threshold=0
   end


   'sim_paper128_hiuvres': begin
	instrument='paper'
	dft_threshold=0
	max_model_sources=100
	kbinsize=0.1
	nfreq_avg=203
	no_extend=1
   end

   'sim_paper128_louvres': begin
	instrument='paper'
	dft_threshold=0
	max_model_sources=100
	kbinsize=0.5
	nfreq_avg=203
	no_extend=1
   end

   'sim_paper128_1hour': begin
	; 1 hour total, in 2 min files
	;simulate_header=1
	instrument='paper'
	dft_threshold=0
	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	beam_offset_time = 60.		; calculate beam model at the center tmie
	max_sources=10000
        max_model_sources=10000
	max_calibration_sources=10000
   end

   'sim_paper128_9hour': begin
	instrument='paper'
	dft_threshold=0
	max_model_sources=10000
	nfreq_avg=203
	
   end

   'sim_paper128_9hour_noextend': begin
	instrument='paper'
	dft_threshold=1
	max_model_sources=10000
	nfreq_avg=203    ;One beam model only
	no_extend=1	;Treat extended sources as point.
   end

   'sim_paper128_9hour_nofornax': begin
	instrument='paper'
	dft_threshold=1
	max_model_sources=10000
	sources_file_name="mwa_calibration_source_list_gleam_kgs_no_fornax"
   end


   'sim_paper128_9hour_1coarse': begin
	instrument='paper'
	dft_threshold=1
 	nfreq_avg=203
	max_model_sources=10000
   end


   'sim_paper128_onlyfornax2': begin
	instrument='paper'
	dft_threshold=1
 	nfreq_avg=203
	no_extend=0
	max_model_sources=1
	max_calibration_sources=1
   end

   'sim_paper128_1src': begin
	instrument='paper'
	dft_threshold=1
	sources_file_name="point_source2_paper"
	reorder_visibilities=1
	max_sources=1
	max_model_sources=1
	max_calibration_sources=1
   end


   'sim_paper128_10000src_diffuse': begin
	instrument='paper'
	dft_threshold=1
	max_sources=10000
	max_model_sources=10000
	max_calibration_sources=10000
        diffuse_file_name='EoR0_diffuse_model.sav'
        diffuse_model = filepath(string(format='(A,".sav")',diffuse_file_name),root=rootdir('FHD'), subdir='catalog_data')
   end

   'sim_ewbase_onesrc': begin
	;simulate_header=1
	instrument='mwa'
	dft_threshold=1
	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	max_sources=1
        max_model_sources=1
	max_calibration_sources=1
   end


   'sim_ewbase_100src': begin
	;simulate_header=1
	instrument='mwa'
	split_ps_export=0
;	beam_offset_time=408     ; 408 seconds. about halfway through one PAPER "obsID"
	dft_threshold=1
;	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	max_sources=100
        max_model_sources=100
	max_calibration_sources=100
   end


   'sim_ewbase_10000src_halftime': begin
	;simulate_header=1
	instrument='mwa'
	split_ps_export=0
;	beam_offset_time=408     ; 408 seconds. about halfway through one PAPER "obsID"
	dft_threshold=1
;	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	max_sources=10000
        max_model_sources=10000
	max_calibration_sources=10000
   end


   'sim_ewbase_phasetest': begin
	instrument='mwa'
	dft_threshold=1
        max_model_sources=10000
   end


   'sim_ewbase_10000src_onecoarse': begin
	;simulate_header=1
	instrument='mwa'
	split_ps_export=0
;	beam_offset_time=408     ; 408 seconds. about halfway through one PAPER "obsID"
	dft_threshold=1
	nfreq_avg=384             ; Fine frequencies per coarse channel.
;	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	max_sources=10000
        max_model_sources=10000
	max_calibration_sources=10000
   end

   'sim_ewbase-mwa_onesrc': begin
	;simulate_header=1
	instrument='mwa'
	dft_threshold=1
	reorder_visibilities=1      ;Necessary for plot_uv to make sense of the results.
	max_sources=1
        max_model_sources=1
	max_calibration_sources=1
   end

   'sim_paper128_test': begin
        ;simulate_header=1
        instrument='paper'
        dft_threshold=1
	max_sources=1
        max_model_sources=1
	max_calibration_sources=1
   end


   'sim_eor_hera19_mwabeam': begin
	eor_sim=1
	save_uvf=1
	max_model_sources=0
	instrument='mwa'
	nfreq_avg=203
	include_catalog_sources=0
   end

   'sim_eor_hera19_test': begin
	eor_sim=1
	max_model_sources=0
	instrument='hera'
	nfreq_avg=203
   end

   'sim_eor_paper128_9hour': begin
	eor_sim=1
	save_uvf=1
	max_model_sources=0
	instrument='paper'
	dft_threshold=0
;        sources_file_name="zem_simulation_sources/sim_source_list1"
	include_catalog_sources=0
	nfreq_avg=203
   end

   'sim_eor_paper128_2': begin
	eor_sim=1
	save_uvf=1
	max_model_sources=0
	instrument='paper'
	dft_threshold=0
;        sources_file_name="zem_simulation_sources/sim_source_list1"
	include_catalog_sources=0
	nfreq_avg=203
   end

   'sim_eor_paper128_at_MRO': begin
	instrument='paper'
	dft_threshold=0
	eor_sim=1
	max_model_sources=0
	include_catalog_sources=0
	nfreq_avg=384
   end

   'sim_eor_mwa128': begin
	eor_sim=1
	max_model_sources=0
	instrument='mwa'
	dft_threshold=0
	dimension=1024
;        sources_file_name="zem_simulation_sources/sim_source_list1"
	include_catalog_sources=0
	nfreq_avg=384    		; 1
   end


   'sim_hera19_9hour': begin
	instrument='hera'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=203
   end

   'sim_heraplat_30min': begin
	instrument='hera'
	max_model_sources=1000
	dft_threshold=1
	nfreq_avg=1024
	n_pol=1    ; Unsure why... but these files only have one pol
   end

   'sim_hera19_9hour_medUVres': begin
	instrument='hera'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=203
	kbinsize=0.1
   end


   'sim_hera19_9hour_hiUVres': begin
	;requires over 120G of memory per job!
	instrument='hera'
	max_model_sources=7000
	dft_threshold=0
	nfreq_avg=203
	kbinsize=0.05
   end


   'test': begin
	include_catalog_sources=0
	max_model_sources=0
	max_calibration_sources=0
	max_sources=0
	instrument='hera'
	dft_threshold=1
	dimension=512
	nfreq_avg=203
	eor_sim=1
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

file_path_fhd = fhd_path_setup(sim_from_uvfits_filepath,output_directory=output_directory,version=version,_Extra=extra) ;Creates output directories for each obsid
temp_path2 = file_path_fhd[0]
undefine, file_path_fhd
file_path_fhd = temp_path2

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
