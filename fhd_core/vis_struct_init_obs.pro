FUNCTION vis_struct_init_obs,header,params, dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    lon=lon,lat=lat,alt=alt,rotation=rotation, data_directory=data_directory, filename=filename,$
    pflag=pflag, n_pol=n_pol,version=version, _Extra=extra
;initializes the structure containing frequently needed parameters relating to the observation
IF N_Elements(data_directory) EQ 0 THEN data_directory='' ;full path and file name of the associated visibility, excluding the '.uvfits' extension
IF N_Elements(filename) EQ 0 THEN filename=''
IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(dimension) EQ 0 THEN dimension=1024. ;dimension of the image in pixels; dimension = x direction
IF N_Elements(elements) EQ 0 THEN elements=dimension ;elements = y direction
IF N_Elements(kbinsize) EQ 0 THEN kbinsize=0.5 ;k-space resolution, in wavelengths per pixel
IF N_Elements(degpix) EQ 0 THEN degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel
IF N_Elements(lon) EQ 0 THEN lon=116.67081 ;degrees
IF N_Elements(lat) EQ 0 THEN lat=-26.703319 ;degrees
IF N_Elements(alt) EQ 0 THEN alt=377.83 ;altitude (meters)
IF N_Elements(pflag) EQ 0 THEN pflag=0

IF N_Elements(obsx) EQ 0 THEN obsx=dimension/2.
IF N_Elements(obsy) EQ 0 THEN obsy=elements/2.

IF Keyword_Set(params) AND Keyword_Set(header) THEN BEGIN
    obsra=header.obsra
    obsdec=header.obsdec
    
    time=params.time
    b0i=Uniq(time)
    time_step=(time[b0i[1]]-time[b0i[0]])*24.*3600.
    time_total=(Max(time)-Min(time))*24.*3600.
    nb=N_Elements(b0i)
    bin_start=fltarr(nb) & bin_start[1:*]=b0i[0:nb-2]+1
    bin_end=b0i
    time_bin=fltarr(2,nb) & time_bin[0,*]=bin_start & time_bin[1,*]=bin_end
    bin_width=fltarr(nb)
    bin_width[0]=b0i[0]+1
    FOR i=1,nb-1 DO bin_width[i]=b0i[i]-b0i[i-1]
    bin_width_c=total(bin_width,/cumulative)
    bin_offset=fltarr(nb) & bin_offset[1:*]=total(bin_width[0:nb-2],/cumulative)    
    
    frequency_array=(findgen(header.n_freq)-header.freq_ref_i)*header.freq_width+header.freq_ref
    freq_bin=32.*header.freq_width  ;Hz
    freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
    nfreq_bin=N_Elements(freq_hist)
    freq_bin_i=fltarr(header.n_freq)
    FOR bin=0,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
    
    year=Float(Strmid(header.date,0,4))
    month=Float(Strmid(header.date,5,2))
    day=Float(Strmid(header.date,8,2))
    jdate=double(header.jd0)+time[b0i]
    epoch=Double(year+ymd2dn(year,month,day)/365.25)

    zenpos2,Min(Jdate),zenra,zendec, lat=lat, lng=lon,/degree,/J2000

    IF Abs(obsra-zenra) GT 90. THEN ra_diff=obsra-((obsra GT zenra) ? 360.:(-360.))-zenra ELSE ra_diff=obsra-zenra
    dec_diff=obsdec-zendec
    lon_diff=ra_diff*Cos(zendec*!DtoR)
    IF N_Elements(rotation) EQ 0 THEN BEGIN
    ;    rotation=-ra_diff*Sin(obsdec*!DtoR)/(1-Sin(dec_diff*!DtoR))
    ;    rotation=-Atan(Sin(lon_diff*!DtoR)/(Cos(lon_diff*!DtoR)*Cos(obsdec*!DtoR)))*!RaDeg ;calculated from rotation matrices
        rotation=-Float(Atan(Sin(Double(lon_diff*!DtoR)),(Cos(Double(lon_diff*!DtoR))*Cos(Double(obsdec*!DtoR))))*!RaDeg) ;calculated from rotation matrices
        rotation*=Sin(zendec*!DtoR)*Cos(dec_diff*!DtoR)^2.
    ENDIF
;    obsra0=obsra
;    obsdec0=obsdec
;    zenra0=zenra
;    zendec0=zendec
;    JPrecess,obsra0,obsdec0,obsra1,obsdec1,epoch=epoch
;    JPrecess,zenra0,zendec0,zenra1,zendec1,epoch=epoch
;    obsra_shift=obsra1-obsra0
;    obsdec_shift=obsdec1-obsdec0
;    zenra_shift=zenra1-zenra0
;    zendec_shift=zendec1-zendec0
;    obsra+=obsra_shift
;    obsdec+=obsdec_shift
;    zenra+=zenra_shift
;    zendec+=zendec_shift

    ;256 tile upper limit is hard-coded in CASA format
    ;these tile numbers have been verified to be correct
    tile_A=Long(Floor(params.baseline_arr/256)) ;tile numbers start from 1
    tile_B=Long(Fix(params.baseline_arr mod 256))
    
    calibration=fltarr(header.n_pol)+1.
    IF N_Elements(n_pol) EQ 0 THEN n_pol=header.n_pol
    n_tile=header.n_tile
    n_freq=header.n_freq
    n_vis=Float(N_Elements(time))*n_freq
    
    kx_arr=params.uu#frequency_array
    ky_arr=params.vv#frequency_array
    kr_arr=Sqrt(Temporary(kx_arr)^2.+Temporary(ky_arr)^2.)
    max_baseline=Max(kr_arr)
    min_baseline=Min(kr_arr[where(kr_arr)])
    
    vis_coordinates,astr=astr,degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
        dimension=dimension,elements=elements,rotation=rotation,obsx=obsx,obsy=obsy,zenx=zenx,zeny=zeny
ENDIF 

IF N_Elements(tile_A) EQ 0 THEN tile_A=lonarr(1) ;tile numbers start from 1
IF N_Elements(tile_B) EQ 0 THEN tile_B=lonarr(1) ;tile numbers start from 1
IF N_Elements(bin_offset) EQ 0 THEN bin_offset=lonarr(1) ;indices to the start of each time integration
IF N_Elements(Jdate) EQ 0 THEN Jdate=fltarr(1) ;Julian date of each time integration
IF N_Elements(frequency_array) EQ 0 THEN frequency_array=fltarr(1) ;full frequency list
IF N_Elements(freq_bin_i) EQ 0 THEN freq_bin_i=lonarr(1) ;bin number of each frequency. The same psf is used for all frequencies with the same bin number
IF N_Elements(zenra) EQ 0 THEN zenra=0. ;degrees
IF N_Elements(zendec) EQ 0 THEN zendec=0. ;degrees
IF N_Elements(rotation) EQ 0 THEN rotation=0. ;degrees
IF N_Elements(obsra) EQ 0 THEN obsra=0. ;degrees
IF N_Elements(obsdec) EQ 0 THEN obsdec=0. ;degrees
IF N_Elements(calibration) EQ 0 THEN calibration=fltarr(1)+1.
IF N_Elements(n_pol) EQ 0 THEN n_pol=0
IF N_Elements(n_tile) EQ 0 THEN n_tile=0.
IF N_Elements(n_freq) EQ 0 THEN n_freq=0.
IF N_Elements(n_vis) EQ 0 THEN n_vis=0L
IF N_Elements(max_baseline) EQ 0 THEN max_baseline=0.
IF N_Elements(zenx) EQ 0 THEN zenx=obsx
IF N_Elements(zeny) EQ 0 THEN zeny=obsy
IF N_Elements(astr) EQ 0 THEN vis_coordinates,astr=astr,degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,rotation=rotation,obsx=obsx,obsy=obsy,zenx=zenx,zeny=zeny

struct={data_directory:data_directory,filename:filename,dimension:dimension,elements:elements,kpix:kbinsize,degpix:degpix,$
    tile_A:tile_A,tile_B:tile_B,bin_offset:bin_offset,Jdate:Jdate,freq:frequency_array,fbin_i:freq_bin_i,$
    obsra:obsra,obsdec:obsdec,zenra:zenra,zendec:zendec,obsx:obsx,obsy:obsy,zenx:zenx,zeny:zeny,lon:lon,lat:lat,alt:alt,rotation:rotation,$
    pflag:pflag,cal:calibration,n_pol:n_pol,n_tile:n_tile,n_freq:n_freq,n_vis:n_vis,version:version,$
    max_baseline:max_baseline,min_baseline:min_baseline,astr:astr}
RETURN,struct
END