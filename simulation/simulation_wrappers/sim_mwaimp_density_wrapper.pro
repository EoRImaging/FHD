PRO sim_mwaimp_density_wrapper, version=version, sources_file_name=sources_file_name, catalog_file_name=catalog_file_name,fov=fov,$
    sim_baseline_density=sim_baseline_density,set_sidelobe_keywords=set_sidelobe_keywords, _Extra=extra


except=!except
!except=0
heap_gc


if n_elements(version) eq 0 then begin ;version provides a name for the output subdirectory
    print, 'Please provide a version designator.'
    return
endif
    
recalculate_all=1 ; If there is no data being used for context, FHD should calculate everything from scratch.
grid_recalculate = 1 ; This is needed in array_simulator to enter the griding/imaging sections

;if n_elements(catalog_file_name) eq 0 then catalog_file_name='master_catalog' 
;catalog_file_path = filepath(string(format='(A,".sav")',catalog_file_name),root=rootdir('FHD'),subdir='catalog_data')

;;** Setting up the source_array structure that contains the sources to be used in the simulation
if n_elements(sources_file_name) eq 0 then begin 
    print, 'Please supply the name of an input file with sources to simulate.'
    return
endif else print, 'Using sources: ' + sources_file_name

sources_file_path = filepath(string(format='(A,".sav")',sources_file_name),root=rootdir('fhd'), subdir='catalog_data/zem_simulation_sources')
restore, sources_file_path
source_array = catalog
undefine, catalog
;;**

output_directory='/data4/MWA/zmart_FHD/mwa_impulse_densitysim' ; output directory for fhd_path_setup

use_obsid = 1 ; toggles using a specific obsid/uvfits file
if keyword_set(use_obsid) then begin
    ; Choose a directory that contains uvfits files; denoted by jdate
    julian_day = 2456528
    uvfits_version=2
    uvfits_subversion=0

    data_directory='/data4/MWA/EoRuvfits/jd'+strtrim(julian_day,2)+'v'+strtrim(uvfits_version,2)+'_'+strtrim(uvfits_subversion,2) ;location of uvfits files
    
    ;vis_file_list=file_search(data_directory,'*.uvfits',count=n_files) ;list of uvfits files in data_directory
    vis_file_list=[data_directory+'/1061316296.uvfits'] ; a specific obsid that has a pointing nearer to (ra,dec) = (0,-26)
    temp_path=vis_file_list[0] ; vis_file_list needs needs to be a scalar each time it is passed to array_simulator. For now, we are only using one file.
    undefine, vis_file_list
    sim_from_uvfits_filepath = temp_path ; used in array_simulator_init.pro
    
    file_path_fhd = fhd_path_setup(sim_from_uvfits_filepath,output_directory=output_directory,version=version,_Extra=extra) ;Creates output directories for each obsid
    temp_path2 = file_path_fhd[0]
    undefine, file_path_fhd
    file_path_fhd = temp_path2
    
endif else begin
    
    file_path_fhd = fhd_path_setup(output_filename='output',output_directory=output_directory,version=version,_Extra=extra)
    simulate_header = 1 ;Must be set if no obsid is provided (use_obsid=0). Used in array_simulate_init.pro to turn on header creation
        ; Simulating a uvfits header speeds things up compared to using a real one (I think)
endelse

healpix_path = fhd_path_setup(output_directory=output_directory, subdir='Healpix', output_filename='Combind_obs', version=version, _Extra=extra)

export_images = 1 ; toggles the output of images from fhd_quickview
save_visibilities= 1
snapshot_healpix_export = 1
split_ps_export=1
save_imagecube=1
save_uvf=1

unflag_all=1
no_frequency_flagging=1

if n_elements(set_sidelobe_keywords) eq 0 then set_sidelobe_keywords=0

allow_sidelobe_image_output=set_sidelobe_keywords
allow_sidelobe_sources=set_sidelobe_keywords
allow_sidelobe_model_sources=set_sidelobe_keywords
allow_sidelobe_cal_sources=set_sidelobe_keywords

include_catalog_sources = 0 ; toggles the use of catalog sources in the simulation source list.


if n_elements(sim_baseline_density) eq 0 then message, "Density not set." ;baseline density in number per wavelength^2 
eor_sim = 0; toggles eor simulation in vis_simulate

;used for simulating baselines

; This is now unnecessary with the structure above. The toggling is now in the use_obsid parameter.
; 
;if ~( keyword_set(use_obsid) xor keyword_set(simulate_header) ) then begin ; Only allows continuation if one or the other is set, but not both.
;    print, 'Error! Either a uvfits obsid must be provided, or uvfits header simulation must be turned on, but not both.'
;    return
;endif

; To work with Bryna's changes for noise simulations, include_noise must be explicitly off. Should be investigated and fixed!
include_noise=0
noise_sigma_freq=0

;Convoluted way of setting up 'instrument' for use here, while still leaving it to be passed in Extra
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'instrument') THEN instrument=extra.instrument
IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
IF N_Elements(double_precison_beam) EQ 0 THEN double_precison_beam=0

;Set up gridding and deconvolution parameters
IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)

n_pol = 2
image_filter_fn='filter_uv_uniform' ; not sure if this makes sense for simulations
dimension=1024
if n_elements(fov) eq 0 then fov=80.
nfreq_avg=384.

psf_resolution=100.
kbinsize=0.5

no_rephase=1 ;set to use obsra, obsdec for phase center even if phasera, phasedec present in a .metafits file

;obsra=0. ; forces an obsra 
;obsdec=-26.78


; Baseline Simulation
IF N_Elements(n_avg) EQ 0 THEN n_avg=2
IF N_Elements(ps_kbinsize) EQ 0 THEN ps_kbinsize=0.5
IF N_Elements(ps_kspan) EQ 0 THEN ps_kspan=600.
    
if keyword_set(sim_baseline_density) then begin
  ;; set up baseline distribution
  simulate_baselines = 1
  
  ps_kspan_x_mod = 1.0
  ps_kspan_y_mod = 1.0
  
  ps_kspan_x = ps_kspan * ps_kspan_x_mod
  ps_kspan_y = ps_kspan * ps_kspan_y_mod
  
  nsample = round(ps_kspan_x * ps_kspan_y * sim_baseline_density, /L64)
  sim_uu = randomu(seed, nsample)*ps_kspan_x - ps_kspan_x/2. ;nsample u components
  sim_vv = randomu(seed, nsample)*ps_kspan_y - ps_kspan_y/2. ;nsample v components, minmax is set by span in kspace
  
  ;; convert to light travel time (ie m/c or wavelenghts/freq) -- use f=150MHz 
  ;for lowest frequency
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
;End Baseline Simulation

array_simulator, vis_arr, vis_weights, obs, status_str, psf, params, jones, $    
    recalculate_all=recalculate_all, $
    grid_recalculate=grid_recalculate, $
    file_path_fhd=file_path_fhd, $
    catalog_file_path=catalog_file_path, $
    sim_from_uvfits_filepath=sim_from_uvfits_filepath, $
    source_array=source_array, $
    include_catalog_sources=include_catalog_sources, $
    error=error, $
    unflag_all=unflag_all, $
    no_frequency_flagging=no_frequency_flagging, $
    eor_sim=eor_sim, $
    simulate_header=simulate_header, $
    complex=complex_beam, $
    double=double_precision_beam, $
    simulate_baselines=simulate_baselines, sim_baseline_uu=sim_baseline_uu, sim_baseline_vv=sim_baseline_vv, n_time=n_time, sim_baseline_time_inds=sim_baseline_time, $
    export_images=export_images, $
    save_visibilities=save_visibilities, $
    snapshot_healpix_export=snapshot_healpix_export, $
    split_ps_export=split_ps_export, $
    save_imagecube=save_imagecube, $
    save_uvf=save_uvf,$
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
    _Extra=extra
    
heap_gc

!except=except

end