FUNCTION fhd_struct_init_antenna,obs,beam_model_version=beam_model_version,$
    psf_resolution=psf_resolution,psf_intermediate_res=psf_intermediate_res,$
    psf_image_resolution=psf_image_resolution,timing=timing,$
    psf_dim=psf_dim,psf_max_dim=psf_max_dim,_Extra=extra
t0=Systime(1)

IF N_Elements(beam_model_version) EQ 0 THEN beam_model_version=1
instrument=obs.instrument
tile_gain_fn=instrument+'_beam_setup_gain' ;mwa_beam_setup_gain
tile_init_fn=instrument+'_beam_setup_init' ;mwa_beam_setup_init
n_tiles=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol
n_ant_pol=2 ;use as default, since almost all instruments have two instrumental polarizations (either linear or circular)

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

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
nbaselines=obs.nbaselines
ant_names=tile_A[uniq(tile_A[0:nbaselines-1],Sort(tile_A[0:nbaselines-1]))]


dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
;kx_span=kbinsize*dimension ;Units are # of wavelengths
;ky_span=kx_span
degpix=obs.degpix
astr=obs.astr

speed_light=299792458. ;speed of light, in meters/second
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32? ;super-resolution factor
IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=10.
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
antenna_str={n_pol:n_ant_pol,antenna_type:instrument,names:ant_names,model_version:beam_model_version,freq:freq_center,nfreq_bin:nfreq_bin,$
    n_ant_elements:0,Jones:Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin),coupling:Ptrarr(n_ant_pol,nfreq_bin),gain:Ptrarr(n_ant_pol),coords:Ptrarr(3),$
    delays:Ptr_new(),size_meters:0.,height:0.,response:Ptrarr(n_ant_pol,nfreq_bin),group_id:Lonarr(n_ant_pol)-1}
    
;update structure with instrument-specific values, and return as a structure array, with an entry for each tile/antenna
;first, update to include basic configuration data
antenna=Call_function(tile_init_fn,obs,antenna_str,_Extra=extra) ;mwa_beam_setup_init

psf_dim=Ceil((Max(antenna.size_meters)*2.*Max(frequency_array)/speed_light)/kbinsize/Cos(obsza*!DtoR))  
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even

IF Keyword_Set(psf_max_dim) THEN BEGIN
    psf_max_dim=Ceil(psf_max_dim/2.)*2 ;dimension MUST be even
    psf_dim=psf_dim<psf_max_dim
ENDIF

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
Eq2Hor,ra_use,dec_use,Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate
za_arr=fltarr(psf_image_dim,psf_image_dim)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_image_dim,psf_image_dim) & az_arr[valid_i]=az_arr1

;now, update antenna structure to include gains
antenna=Call_function(tile_gain_fn,obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,_Extra=extra) ;mwa_beam_setup_gain

;Finally, update antenna structure to include the response of each antenna
antenna=general_antenna_response(obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim)

timing=Systime(1)-t0
RETURN,antenna
END