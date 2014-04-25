FUNCTION fhd_struct_init_obs,file_path_vis,hdr,params, dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    lon=lon,lat=lat,alt=alt, pflag=pflag, n_pol=n_pol,max_baseline=max_baseline,min_baseline=min_baseline,$
    FoV=FoV,precess=precess,rotate_uv=rotate_uv,scale_uv=scale_uv,mirror_X=mirror_X,mirror_Y=mirror_Y,$
    zenra=zenra,zendec=zendec,phasera=phasera,phasedec=phasedec,obsx=obsx,obsy=obsy,instrument=instrument,$
    nfreq_avg=nfreq_avg,freq_bin=freq_bin,time_cut=time_cut,spectral_index=spectral_index,$
    psf_dim=psf_dim,antenna_size=antenna_size,_Extra=extra

;initializes the structure containing frequently needed parameters relating to the observation
IF N_Elements(pflag) EQ 0 THEN pflag=0
IF N_Elements(spectral_index) EQ 0 THEN spectral_index=-0.8 
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)
IF N_Elements(antenna_size) EQ 0 THEN antenna_size=4. ;meters (MWA groundscreen size)
obsname=file_basename(file_basename(file_path_vis,'.uvfits',/fold_case),'_cal',/fold_case)
git,'describe',result=code_version,project='fhd'
IF N_Elements(code_version) GT 0 THEN code_version=code_version[0] ELSE code_version=''

speed_light=299792458.
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
bin_offset=fltarr(nb) & bin_offset[1:*]=total(bin_width[0:nb-2],/cumulative)    
time_use=Fltarr(nb)+1
FOR ti=0,N_Elements(time_cut)<2-1 DO BEGIN
    ;time cut is specified in seconds to cut (rounded up to next time integration point). 
    ;Specify negative time_cut to cut time off the end. Specify a vector to cut at both the start and end
    IF time_cut[ti] LT 0 THEN BEGIN
        ti_start=(nb-Ceil(Abs(time_cut[ti])/time_step)-1)>0
        ti_end=nb-1
    ENDIF ELSE BEGIN
        ti_start=0
        ti_end=(Ceil(Abs(time_cut[ti])/time_step))<(nb-1)
    ENDELSE
    time_use[ti_start:ti_end]=0
ENDFOR

frequency_array=(findgen(hdr.n_freq)-hdr.freq_ref_i)*hdr.freq_width+hdr.freq_ref
IF N_Elements(nfreq_avg) EQ 0 THEN BEGIN
    nfreq_avg=Round(hdr.freq_ref/hdr.freq_width/1000.)
ENDIF

IF N_Elements(freq_bin) EQ 0 THEN freq_bin=nfreq_avg*hdr.freq_width  ;Hz
freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
nfreq_bin=N_Elements(freq_hist)
freq_bin_i=fltarr(hdr.n_freq)
FOR bin=0,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
freq_center=Median(frequency_array)

IF Keyword_Set(scale_uv) THEN BEGIN
    params.uu*=scale_uv
    params.vv*=scale_uv
    params.ww*=scale_uv
ENDIF
IF Keyword_Set(rotate_uv) THEN BEGIN
    uu1=(uu=params.uu)
    vv1=(vv=params.vv)
    rotation_arr=fltarr(nb)
    FOR i=0,nb-1 DO BEGIN
        zenpos2,Jdate[i],zenra2,zendec2, lat=lat, lng=lon,/degree,/J2000
        rotation_arr[i]=angle_difference(zendec,zenra,zendec2,zenra2,/degree);/2.
        uu1[bin_start[i]:bin_end[i]]=uu[bin_start[i]:bin_end[i]]*Cos(rotation_arr[i]*!DtoR)-vv[bin_start[i]:bin_end[i]]*Sin(rotation_arr[i]*!DtoR)
        vv1[bin_start[i]:bin_end[i]]=vv[bin_start[i]:bin_end[i]]*Cos(rotation_arr[i]*!DtoR)+uu[bin_start[i]:bin_end[i]]*Sin(rotation_arr[i]*!DtoR)
    ENDFOR
    params.uu=uu1
    params.vv=vv1        
ENDIF

calibration=fltarr(4)+1.
IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol
n_tile=hdr.n_tile
n_freq=hdr.n_freq
n_vis=(n_vis_raw=(n_vis_in=(Float(N_Elements(time))*n_freq)))
n_vis_arr=Lonarr(n_freq)

;256 tile upper limit is hard-coded in CASA format
;these tile numbers have been verified to be correct
tile_A=Long(Floor(params.baseline_arr/256)) ;tile numbers start from 1
tile_B=Long(Fix(params.baseline_arr mod 256))
IF (max(tile_A)>max(tile_B)) NE n_tile THEN BEGIN
    print,String(format='("Mis-matched n_tiles! Header: ",A," vs data: ",A)',Strn(n_tile),Strn(max(tile_A)>max(tile_B)))
    n_tile=max(tile_A)>max(tile_B)
ENDIF
freq_use=Lonarr(n_freq)+1
tile_use=Lonarr(n_tile)+1

kx_arr=params.uu#frequency_array
ky_arr=params.vv#frequency_array
kr_arr=Sqrt((kx_arr)^2.+(ky_arr)^2.)
IF N_Elements(max_baseline) EQ 0 THEN max_baseline_use=Max(Abs(kx_arr))>Max(Abs(ky_arr)) $
    ELSE max_baseline_use=max_baseline

;psf_dim=Ceil((obs.antenna_size*2.*Max(frequency_array)/speed_light)/kbinsize)+1
;psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even
IF Keyword_Set(psf_dim) THEN BEGIN
    psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even
    kbinsize=(antenna_size*2.*Max(frequency_array)/speed_light)/(psf_dim-1.)
    FoV=!RaDeg/kbinsize
ENDIF
IF Keyword_Set(FoV) THEN kbinsize=!RaDeg/FoV
IF ~Keyword_Set(kbinsize) THEN kbinsize=0.5 ;k-space resolution, in wavelengths per pixel
IF N_Elements(degpix) EQ 0 THEN k_span=2.*max_baseline_use ELSE k_span=!RaDeg/degpix 
dimension_test=2.^Round(ALOG10(k_span/kbinsize)/ALOG10(2.))

IF N_Elements(dimension) EQ 0 THEN dimension=dimension_test ELSE dimension=Float(dimension);dimension of the image in pixels; dimension = x direction
IF N_Elements(elements) EQ 0 THEN elements=dimension ELSE elements=Float(elements);elements = y direction
degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel
IF N_Elements(max_baseline) EQ 0 THEN $
    max_baseline=Max(Abs(kr_arr[where((Abs(kx_arr)/kbinsize LT dimension/2) AND (Abs(ky_arr)/kbinsize LT elements/2))])) $
    ELSE max_baseline=max_baseline<Max(Abs(kr_arr[where((Abs(kx_arr)/kbinsize LT dimension/2) AND (Abs(ky_arr)/kbinsize LT elements/2))]))
IF N_Elements(min_baseline) EQ 0 THEN min_baseline=Min(kr_arr[where(kr_arr)]) ELSE min_baseline=min_baseline>Min(kr_arr[where(kr_arr)])
kx_arr=0 & ky_arr=0 & kr_arr=0 ;free memory
noise_arr=Ptr_new()

meta=fhd_struct_init_meta(file_path_vis,hdr,params,degpix=degpix,dimension=dimension,elements=elements,meta_data=meta_data,meta_hdr=meta_hdr,_Extra=extra)
IF N_Elements(meta_data) EQ 0 THEN meta_data=Ptr_new() ELSE meta_data=Ptr_new(meta_data)
IF N_Elements(meta_hdr) EQ 0 THEN meta_hdr=Ptr_new() ELSE meta_hdr=Ptr_new(meta_hdr)

tile_use1=intarr(n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    tile_use_i=where(*(meta.tile_flag[pol_i]) EQ 0,n_use)
    IF n_use GT 0 THEN tile_use1[tile_use_i]+=1
ENDFOR
tile_flag_i=where(tile_use1 EQ 0,n_flag)
IF n_flag GT 0 THEN tile_use[tile_flag_i]=0

arr={tile_A:tile_A,tile_B:tile_B,bin_offset:bin_offset,Jdate:meta.Jdate,freq:frequency_array,fbin_i:freq_bin_i,$
    freq_use:freq_use,tile_use:tile_use,time_use:time_use,tile_names:meta.tile_names,tile_height:meta.tile_height,tile_flag:meta.tile_flag}
struct={code_version:String(code_version),instrument:String(instrument),antenna_size:Float(antenna_size),obsname:String(obsname),$
    dimension:Float(dimension),elements:Float(elements),$
    kpix:Float(kbinsize),degpix:Float(degpix),obsaz:meta.obsaz,obsalt:meta.obsalt,obsra:meta.obsra,obsdec:meta.obsdec,$
    zenra:meta.zenra,zendec:meta.zendec,obsx:meta.obsx,obsy:meta.obsy,zenx:meta.zenx,zeny:meta.zeny,$
    phasera:meta.phasera,phasedec:meta.phasedec,orig_phasera:meta.orig_phasera,orig_phasedec:meta.orig_phasedec,$
    n_pol:Fix(n_pol,type=2),n_tile:Long(n_tile),n_freq:Long(n_freq),$
    n_vis:Long(n_vis),n_vis_in:Long(n_vis_in),n_vis_raw:Long(n_vis_raw),nf_vis:Long(n_vis_arr),$
    jd0:meta.jd0,max_baseline:Float(max_baseline),min_baseline:Float(min_baseline),delays:meta.delays,lon:meta.lon,lat:meta.lat,alt:meta.alt,$
    freq_center:Float(freq_center),astr:meta.astr,alpha:Float(spectral_index),pflag:Fix(pflag,type=2),cal:Float(calibration),$
    residual:0,vis_noise:noise_arr,baseline_info:Ptr_new(arr),meta_data:meta_data,meta_hdr:meta_hdr}    
RETURN,struct
END