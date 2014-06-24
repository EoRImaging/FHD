FUNCTION fhd_struct_init_antenna,obs,beam_model_version=beam_model_version,$
    Jones_matrix_arr=Jones_matrix_arr,_Extra=extra

IF Tag_exist(obs,'instrument') THEN instrument=obs.instrument ELSE instrument='mwa'
tile_gain_fn=instrument+'_beam_setup_gains' ;mwa_beam_setup_gain
tile_init_fn=instrument+'_beam_setup_init' ;mwa_beam_setup_init
n_tiles=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol

obsra=obs.obsra
obsdec=obs.obsdec
zenra=obs.zenra
zendec=obs.zendec
;phasera=obs.phasera
;phasedec=obs.phasedec
Jdate=obs.Jd0
frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

;tile_A=(*obs.baseline_info).tile_A
;tile_B=(*obs.baseline_info).tile_B
;bin_offset=(*obs.baseline_info).bin_offset
;nbaselines=bin_offset[1]

dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
;kx_span=kbinsize*dimension ;Units are # of wavelengths
;ky_span=kx_span
degpix=obs.degpix
astr=obs.astr
IF tag_exist(obs,'delays') THEN delay_settings=obs.delays ;delays need to be generalized!

speed_light=299792458. ;speed of light, in meters/second
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32? ;super-resolution factor
;IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=10.
Eq2Hor,obsra,obsdec,Jdate,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
obsalt=Float(obsalt)
obsaz=Float(obsaz)
obsza=90.-obsalt

freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO BEGIN
    fi_i=where(freq_bin_i EQ fi,n_fi)
    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) ELSE freq_center[fi]=Median(frequency_array[fi_i])
ENDFOR

;initialize antenna structure
antenna_str={antenna_type:instrument,model_version:beam_model_version,freq:freq_center,height:0.,group_id:0,$
    n_ant_elements:0,Jones:Ptr_new(),coupling:Ptrarr(n_ant_pol,nfreq_bin),gain_complex:Ptrarr(2),coords:Ptrarr(3),$
    size_meters:antenna_size}
    
;update structure with instrument-specific values, and return as a structure array, with an entry for each tile/antenna
;first, update to include basic configuration data
antenna=Call_function(tile_init_fn,obs,antenna_str,_Extra=extra) ;mwa_beam_setup_init

psf_dim=Ceil((Max(antenna.size_meters)*2.*Max(frequency_array)/speed_light)/kbinsize/Cos(obsza*!DtoR))  
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even

psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model
psf_superres_dim=psf_dim*psf_resolution
psf_scale=dimension*psf_intermediate_res/psf_image_dim

;xvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,1)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
;yvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,2)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)

xvals_celestial=meshgrid(psf_image_dim,psf_image_dim,1)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
yvals_celestial=meshgrid(psf_image_dim,psf_image_dim,2)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
xy2ad,xvals_celestial,yvals_celestial,astr,ra_arr,dec_arr
valid_i=where(Finite(ra_arr),n_valid)
ra_use=ra_arr[valid_i]
dec_use=dec_arr[valid_i]

;NOTE: Eq2Hor REQUIRES Jdate to have the same number of elements as RA and Dec for precession!!
;;NOTE: The NEW Eq2Hor REQUIRES Jdate to be a scalar! They created a new bug when they fixed the old one
Eq2Hor,ra_use,dec_use,Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
za_arr=fltarr(psf_image_dim,psf_image_dim)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_image_dim,psf_image_dim) & az_arr[valid_i]=az_arr1

xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)

;now, update antenna structure to include gains
antenna=Call_function(tile_gain_fn,obs,antenna_str,_Extra=extra) ;mwa_beam_setup_gain

RETURN,antenna
END