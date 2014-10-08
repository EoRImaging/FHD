FUNCTION fhd_struct_init_obs,file_path_vis,hdr,params, dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    pflag=pflag, n_pol=n_pol,max_baseline=max_baseline,min_baseline=min_baseline,$
    FoV=FoV,rotate_uv=rotate_uv,scale_uv=scale_uv,mirror_X=mirror_X,mirror_Y=mirror_Y,$
    zenra=zenra,zendec=zendec,phasera=phasera,phasedec=phasedec,obsx=obsx,obsy=obsy,instrument=instrument,$
    nfreq_avg=nfreq_avg,freq_bin=freq_bin,time_cut=time_cut,spectral_index=spectral_index,$
    psf_dim=psf_dim,nside=nside,restrict_hpx_inds=restrict_hpx_inds,$
    n_hpx=n_hpx,n_zero_hpx=n_zero_hpx,_Extra=extra

;initializes the structure containing frequently needed parameters relating to the observation
IF N_Elements(pflag) EQ 0 THEN pflag=0
IF N_Elements(spectral_index) EQ 0 THEN spectral_index=-0.8 
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)
obsname=file_basename(file_basename(file_path_vis,'.uvfits',/fold_case),'_cal',/fold_case)
git,'describe',result=code_version,repo_path=rootdir('fhd'),args='--long'
IF N_Elements(code_version) GT 0 THEN code_version=code_version[0] ELSE code_version=''

speed_light=299792458.
time=params.time
b0i=Uniq(time)
n_time=N_Elements(b0i)
IF n_time GT 1 THEN time_step=(time[b0i[1]]-time[b0i[0]])*24.*3600. ELSE time_step=1. ;have to put something in if there is only one time interval
time_total=(Max(time)-Min(time))*24.*3600.
bin_start=fltarr(n_time) & IF n_time GT 1 THEN bin_start[1:*]=b0i[0:n_time-2]+1
bin_end=b0i
time_bin=fltarr(2,n_time) & time_bin[0,*]=bin_start & time_bin[1,*]=bin_end
bin_width=fltarr(n_time)
IF n_time GT 1 THEN bin_width[0]=b0i[0]+1 ELSE bin_width[0]=N_Elements(time)
FOR i=1,n_time-1 DO bin_width[i]=b0i[i]-b0i[i-1]
bin_offset=Lonarr(n_time) & IF n_time GT 1 THEN bin_offset[1:*]=total(bin_width[0:n_time-2],/cumulative)    
nbaselines=bin_width[0]
time_use=intarr(n_time)+1
FOR ti=0,N_Elements(time_cut)<2-1 DO BEGIN
    ;time cut is specified in seconds to cut (rounded up to next time integration point). 
    ;Specify negative time_cut to cut time off the end. Specify a vector to cut at both the start and end
    IF time_cut[ti] LT 0 THEN BEGIN
        ti_start=((n_time-Ceil(Abs(time_cut[ti])/time_step))>0)<(n_time-1)
        ti_end=n_time-1
    ENDIF ELSE BEGIN
        ti_start=0
        ti_end=(Ceil(Abs(time_cut[ti])/time_step)-1)<(n_time-1)
    ENDELSE
    time_use[ti_start:ti_end]=0
ENDFOR

freq_res=hdr.freq_width
;frequency_array=(findgen(hdr.n_freq)-(hdr.freq_ref_i-1))*freq_res+hdr.freq_ref ;FITS header indices start at 1
frequency_array=(findgen(hdr.n_freq)-(hdr.freq_ref_i))*freq_res+hdr.freq_ref ;LEAVE unchanged for now to allow comparison!
IF N_Elements(nfreq_avg) EQ 0 THEN nfreq_avg=1.

IF N_Elements(freq_bin) EQ 0 THEN freq_bin=nfreq_avg*freq_res  ;Hz
freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
nfreq_bin=N_Elements(freq_hist)
freq_bin_i=fltarr(hdr.n_freq)
FOR bin=0,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
freq_center=Median(frequency_array)

;IF Keyword_Set(scale_uv) THEN BEGIN
;    params.uu*=scale_uv
;    params.vv*=scale_uv
;    params.ww*=scale_uv
;ENDIF
;IF Keyword_Set(rotate_uv) THEN BEGIN
;    uu1=(uu=params.uu)
;    vv1=(vv=params.vv)
;    rotation_arr=fltarr(n_time)
;    FOR i=0,n_time-1 DO BEGIN
;        zenpos2,Jdate[i],zenra2,zendec2, lat=lat, lng=lon,/degree,/J2000
;        rotation_arr[i]=angle_difference(zendec,zenra,zendec2,zenra2,/degree);/2.
;        uu1[bin_start[i]:bin_end[i]]=uu[bin_start[i]:bin_end[i]]*Cos(rotation_arr[i]*!DtoR)-vv[bin_start[i]:bin_end[i]]*Sin(rotation_arr[i]*!DtoR)
;        vv1[bin_start[i]:bin_end[i]]=vv[bin_start[i]:bin_end[i]]*Cos(rotation_arr[i]*!DtoR)+uu[bin_start[i]:bin_end[i]]*Sin(rotation_arr[i]*!DtoR)
;    ENDFOR
;    params.uu=uu1
;    params.vv=vv1        
;ENDIF

calibration=fltarr(4)+1.
IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol
n_tile=hdr.n_tile
n_freq=hdr.n_freq
n_vis=(n_vis_raw=(n_vis_in=(Float(N_Elements(time))*n_freq)))
n_vis_arr=Lonarr(n_freq)

;256 tile upper limit is hard-coded in CASA format
;these tile numbers have been verified to be correct
name_mod=2.^((Ceil(Alog(Sqrt(nbaselines*2.-n_tile))/Alog(2.)))>Floor(Alog(Min(params.baseline_arr))/Alog(2.)))
tile_A=Long(Floor(params.baseline_arr/name_mod)) ;tile numbers start from 1
tile_B=Long(Fix(params.baseline_arr mod name_mod))
IF (max(tile_A)>max(tile_B)) NE n_tile THEN BEGIN
    print,String(format='("Mis-matched n_tiles! Header: ",A," vs data: ",A)',Strn(n_tile),Strn(max(tile_A)>max(tile_B)))
    n_tile=max(tile_A)>max(tile_B)
ENDIF
;IF (max(tile_A)) NE n_tile THEN BEGIN
;    print,String(format='("Mis-matched n_tiles! Header: ",A," vs data: ",A)',Strn(n_tile),Strn(max(tile_A)))
;    n_tile=max(tile_A)
;ENDIF
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

meta=fhd_struct_init_meta(file_path_vis,hdr,params,degpix=degpix,dimension=dimension,elements=elements,$
    n_tile=n_tile,instrument=instrument,meta_data=meta_data,meta_hdr=meta_hdr,_Extra=extra)
IF N_Elements(meta_data) EQ 0 THEN meta_data=Ptr_new() ELSE meta_data=Ptr_new(meta_data)
IF N_Elements(meta_hdr) EQ 0 THEN meta_hdr=Ptr_new() ELSE meta_hdr=Ptr_new(meta_hdr)

tile_use1=intarr(n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    tile_use_i=where(*(meta.tile_flag[pol_i]) EQ 0,n_use)
    IF n_use GT 0 THEN tile_use1[tile_use_i]+=1
ENDFOR
tile_flag_i=where(tile_use1 EQ 0,n_flag)
IF n_flag GT 0 THEN tile_use[tile_flag_i]=0

IF N_Elements(nside) EQ 0 THEN nside=0
IF N_Elements(restrict_hpx_inds) NE 1 THEN ind_list="UNSPECIFIED" ELSE ind_list=restrict_hpx_inds
IF N_Elements(n_hpx) EQ 0 THEN n_hpx=0
IF N_Elements(n_zero_hpx) EQ 0 THEN n_zero_hpx=-1
pol_names=['XX','YY','XY','YX','I','Q','U','V']
healpix={nside:Long(nside),ind_list:String(ind_list),n_pix:Long(n_hpx),n_zero:Long(n_zero_hpx)}

arr={tile_A:Long(tile_A),tile_B:Long(tile_B),bin_offset:Long(bin_offset),Jdate:meta.Jdate,freq:Float(frequency_array),fbin_i:Long(freq_bin_i),$
    freq_use:Fix(freq_use),tile_use:Fix(tile_use),time_use:Fix(time_use),tile_names:String(meta.tile_names),tile_height:Float(meta.tile_height),tile_flag:meta.tile_flag}
struct={code_version:String(code_version),instrument:String(instrument),obsname:String(obsname),$
    dimension:Float(dimension),elements:Float(elements),nbaselines:Long(nbaselines),$
    kpix:Float(kbinsize),degpix:Float(degpix),obsaz:meta.obsaz,obsalt:meta.obsalt,obsra:meta.obsra,obsdec:meta.obsdec,$
    zenra:meta.zenra,zendec:meta.zendec,obsx:meta.obsx,obsy:meta.obsy,zenx:meta.zenx,zeny:meta.zeny,$
    phasera:meta.phasera,phasedec:meta.phasedec,orig_phasera:meta.orig_phasera,orig_phasedec:meta.orig_phasedec,$
    n_pol:Fix(n_pol,type=2),n_tile:Long(n_tile),n_tile_flag:Long(n_flag),n_freq:Long(n_freq),n_freq_flag:0L,n_time:Long(n_time),n_time_flag:0L,$
    n_vis:Long(n_vis),n_vis_in:Long(n_vis_in),n_vis_raw:Long(n_vis_raw),nf_vis:Long(n_vis_arr),pol_names:pol_names,$
    jd0:meta.jd0,max_baseline:Float(max_baseline),min_baseline:Float(min_baseline),delays:meta.delays,lon:meta.lon,lat:meta.lat,alt:meta.alt,$
    freq_center:Float(freq_center),freq_res:Float(freq_res),astr:meta.astr,alpha:Float(spectral_index),pflag:Fix(pflag,type=2),cal:Float(calibration),$
    residual:0,vis_noise:noise_arr,baseline_info:Ptr_new(arr),meta_data:meta_data,meta_hdr:meta_hdr,healpix:healpix}    
RETURN,struct
END