FUNCTION fhd_struct_init_obs,file_path_vis,hdr,params,layout,dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    n_pol=n_pol,max_baseline=max_baseline,min_baseline=min_baseline,double_precision=double_precision,$
    FoV=FoV,rotate_uv=rotate_uv,scale_uv=scale_uv,mirror_X=mirror_X,mirror_Y=mirror_Y,$
    zenra=zenra,zendec=zendec,phasera=phasera,phasedec=phasedec,obsx=obsx,obsy=obsy,instrument=instrument,$
    beam_nfreq_avg=beam_nfreq_avg,freq_bin=freq_bin,time_cut=time_cut,spectral_index=spectral_index,$
    dft_threshold=dft_threshold,psf_dim=psf_dim,nside=nside,restrict_hpx_inds=restrict_hpx_inds,$
    n_hpx=n_hpx,n_zero_hpx=n_zero_hpx,antenna_mod_index=antenna_mod_index,$
    degrid_spectral_terms=degrid_spectral_terms,grid_spectral_terms=grid_spectral_terms,$
    grid_nfreq_avg=grid_nfreq_avg,_Extra=extra
;initializes the structure containing frequently needed parameters relating to the observation
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)
obsname=file_basename(file_basename(file_path_vis,'.uvfits',/fold_case),'_cal',/fold_case)
git,'describe',result=code_version,repo_path=rootdir('fhd'),args='--long --dirty'
IF N_Elements(code_version) GT 0 THEN code_version=code_version[0] ELSE code_version=''

IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol
n_tile=hdr.n_tile
n_freq=hdr.n_freq

speed_light=299792458. 
time=params.time
b0i=Uniq(time)
n_time=N_Elements(b0i)
time_total=(Max(time)-Min(time))*24.*3600.
bin_start=fltarr(n_time) & IF n_time GT 1 THEN bin_start[1:*]=b0i[0:n_time-2]+1
bin_end=b0i
time_bin=fltarr(2,n_time) & time_bin[0,*]=bin_start & time_bin[1,*]=bin_end
bin_width=fltarr(n_time)
IF n_time GT 1 THEN bin_width[0]=b0i[0]+1 ELSE bin_width[0]=N_Elements(time)
FOR i=1,n_time-1 DO bin_width[i]=b0i[i]-b0i[i-1]
bin_offset=Lonarr(n_time) & IF n_time GT 1 THEN bin_offset[1:*]=total(bin_width[0:n_time-2],/cumulative)    
nbaselines=bin_width[0]
n_vis=(n_vis_raw=(n_vis_in=(Float(N_Elements(time))*n_freq)))
n_vis_arr=Lonarr(2,n_freq)

freq_res=hdr.freq_res
frequency_array=hdr.freq_arr
IF N_Elements(beam_nfreq_avg) EQ 0 THEN beam_nfreq_avg=1.

IF N_Elements(freq_bin) EQ 0 THEN freq_bin=beam_nfreq_avg*freq_res  ;Hz
freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
nfreq_bin=N_Elements(freq_hist)
freq_bin_i=fltarr(n_freq)
FOR bin=0,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
freq_center=Median(frequency_array)

IF Keyword_Set(grid_nfreq_avg) THEN BEGIN
    IF grid_nfreq_avg LT 0 THEN grid_bin_i=freq_bin_i ELSE BEGIN
        IF grid_nfreq_avg LT 1E5 THEN freq_bin=grid_nfreq_avg*freq_res  ELSE freq_bin=grid_nfreq_avg;Hz
        freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
        nfreq_bin=N_Elements(freq_hist)
        grid_bin_i=fltarr(n_freq)
        FOR bin=0,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN grid_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
    ENDELSE
    grid_freq_arr=Fltarr(nfreq_bin)
    FOR f_i=0L,nfreq_bin-1 DO grid_freq_arr[f_i]=Mean(frequency_array[where(grid_bin_i EQ f_i)])
    grid_info=Ptr_new({n_freq:nfreq_bin,freq:grid_freq_arr,bin_i:grid_bin_i})
ENDIF ELSE BEGIN
    grid_info=Ptr_new()
ENDELSE

IF Tag_exist(params, "antenna1") AND Tag_exist(params, "antenna2") THEN BEGIN
    ant1_arr = params.antenna1
    ant2_arr = params.antenna2
    tile_A = ant1_arr
    tile_B = ant2_arr
    antenna_flag=0 
ENDIF ELSE antenna_flag = 1
IF antenna_flag THEN BEGIN
    ;256 tile upper limit is hard-coded in CASA format
    ;these tile numbers have been verified to be correct
    IF not Keyword_Set(antenna_mod_index) THEN BEGIN
        antenna_mod_index_use=Long(2^Floor(Alog(min(params.baseline_arr))/Alog(2.))) 
        tile_B_test=min(params.baseline_arr) mod antenna_mod_index_use
        IF tile_B_test GT 1 THEN $ ; Check if a bad fit
            IF min(params.baseline_arr) mod 2 EQ 1 THEN $ ; but not if autocorrelations or the first tile are missing
                antenna_mod_index_use/=Long(2^Floor(Alog(tile_B_test)/Alog(2.))) 
    ENDIF ELSE antenna_mod_index_use=antenna_mod_index 
    ;antenna_mod_index_use=2.^((Ceil(Alog(Sqrt(nbaselines*2.-n_tile))/Alog(2.)))>Floor(Alog(Min(params.baseline_arr))/Alog(2.)))
    tile_A=Long(Floor(params.baseline_arr/antenna_mod_index_use)) ;tile numbers start from 1
    tile_B=Long(Fix(params.baseline_arr mod antenna_mod_index_use))
    IF (max(tile_A)>max(tile_B)) NE n_tile THEN BEGIN
        print,String(format='("Mis-matched n_tiles! Header: ",A," vs data: ",A)',Strn(n_tile),Strn(max(tile_A)>max(tile_B)))
        n_tile=max(tile_A)>max(tile_B)
    ENDIF
    params.antenna1 = tile_A
    params.antenna2 = tile_B
ENDIF

freq_use=Lonarr(n_freq)+1
tile_use=Lonarr(n_tile)+1

;Calculate kx and ky for each baseline at high precision to get most accurate observation information
kx_arr=params.uu#frequency_array
ky_arr=params.vv#frequency_array
kr_arr=Sqrt((kx_arr)^2.+(ky_arr)^2.)
IF N_Elements(max_baseline) EQ 0 THEN max_baseline_use=Max(Abs(kx_arr))>Max(Abs(ky_arr)) $
    ELSE max_baseline_use=max_baseline

;Determine the imaging parameters to use
IF Keyword_Set(FoV) AND Keyword_Set(kbinsize) THEN $
    print,"WARNING!! Only one of FoV and kbinsize can be specified. Using FoV."
IF Keyword_Set(FoV) THEN kbinsize=!RaDeg/FoV

;Determine observation resolution/extent parameters given number of pixels in x direction (dimension)
IF Keyword_Set(dimension) THEN BEGIN
    IF Keyword_Set(kbinsize) THEN BEGIN
        IF Keyword_Set(degpix) THEN print, "WARNING! Imaging parameters over constrained. Ignoring degpix."
        degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel
    ENDIF ELSE BEGIN
        IF ~Keyword_Set(degpix) THEN BEGIN
            kbinsize=0.5 ;k-space resolution, in wavelengths per pixel
            degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel
        ENDIF ELSE kbinsize = !RaDeg/(degpix*dimension)
    ENDELSE
ENDIF ELSE BEGIN
    IF ~Keyword_Set(kbinsize) THEN kbinsize=0.5 ;k-space resolution, in wavelengths per pixel        
    IF ~Keyword_Set(degpix) THEN k_span=2.*max_baseline_use ELSE k_span=!RaDeg/degpix 
    dimension=(elements=2.^Round(ALOG10(k_span/kbinsize)/ALOG10(2.)))
ENDELSE
IF ~Keyword_Set(elements) THEN elements=dimension

;Determine the maximum and minimum baseline (cross-correlations only) to use for the given extent
IF N_Elements(max_baseline) EQ 0 THEN $
    max_baseline=Max(Abs(kr_arr[where((Abs(kx_arr)/kbinsize LT dimension/2) AND (Abs(ky_arr)/kbinsize LT elements/2))])) $
    ELSE max_baseline=max_baseline<Max(Abs(kr_arr[where((Abs(kx_arr)/kbinsize LT dimension/2) AND (Abs(ky_arr)/kbinsize LT elements/2))]))
IF N_Elements(min_baseline) EQ 0 THEN min_baseline=Min(kr_arr[where(kr_arr)]) ELSE min_baseline=min_baseline>Min(kr_arr[where(kr_arr)])
kx_arr=0 & ky_arr=0 & kr_arr=0 ;free memory
noise_arr=Ptr_new()

; check that all elements in the antenna1 and antenna2 array exist in the antenna numbers
; from the uvfits antenna table
all_ants = [params.antenna1, params.antenna2]
uniq_ants = all_ants[uniq(all_ants)]
for i=0, n_elements(uniq_ants)-1 do begin
    ind = where(uniq_ants[i] EQ (layout.antenna_numbers), n_count)
    if n_count EQ 0 then message, "antenna arrays contain number(s) not found in antenna table"
endfor

; fhd expects antenna1 and antenna2 arrays containing indices that are one-indexed. 
; Some uvfits files contain actual antenna numbers in these fields, while others  
; (particularly, those written by cotter or birli) contain indices.
; To account for this, all antenna numbers from the uvfits header are mapped to indices 
; using the antenna numbers from the uvfits antenna table.
; If the antenna numbers were written into the file as indices, they will be mapped to themselves.
tile_A = params.antenna1
tile_B = params.antenna2
for tile_i=0, n_tile-1 do begin
    inds = where(layout.antenna_numbers[tile_i] EQ (params.antenna1),n_count)
    if n_count GT 0 then tile_A[inds] = tile_i+1
    inds = where(layout.antenna_numbers[tile_i] EQ (params.antenna2),n_count)
    if n_count GT 0 then tile_B[inds] = tile_i+1
endfor
params.antenna1 = tile_A
params.antenna2 = tile_B

meta=fhd_struct_init_meta(file_path_vis,hdr,params,layout,degpix=degpix,dimension=dimension,elements=elements,$
    n_tile=n_tile,instrument=instrument,meta_data=meta_data,meta_hdr=meta_hdr,_Extra=extra)

IF N_Elements(meta_data) EQ 0 THEN meta_data=Ptr_new() ELSE meta_data=Ptr_new(meta_data)
IF N_Elements(meta_hdr) EQ 0 THEN meta_hdr=Ptr_new() ELSE meta_hdr=Ptr_new(meta_hdr)

time_use=intarr(n_time)+1
FOR ti=0,N_Elements(time_cut)<2-1 DO BEGIN
    ;time cut is specified in seconds to cut (rounded up to next time integration point). 
    ;Specify negative time_cut to cut time off the end. Specify a vector to cut at both the start and end
    IF time_cut[ti] LT 0 THEN BEGIN
        ti_start=((n_time-Ceil(Abs(time_cut[ti])/meta.time_res))>0)<(n_time-1)
        ti_end=n_time-1
    ENDIF ELSE BEGIN
        ti_start=0
        ti_end=(Ceil(Abs(time_cut[ti])/meta.time_res)-1)<(n_time-1)
    ENDELSE
    IF ti_end GE ti_start THEN time_use[ti_start:ti_end]=0
ENDFOR
n_time_cut = n_time - Total(time_use)

;TILE FLAGGING HAPPENS HERE FOR TILE_USE, IF FLAGGED IN EITHER POL THEN FLAGGED IN BOTH POLS
tile_use1=intarr(n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    tile_use_i=where(*(meta.tile_flag[pol_i]) EQ 0,n_use)
    IF n_use GT 0 THEN tile_use1[tile_use_i]+=1
ENDFOR
tile_flag_i=where(tile_use1 EQ 0,n_flag)
IF n_flag GT 0 THEN tile_use[tile_flag_i]=0

IF N_Elements(degrid_spectral_terms) EQ 0 THEN degrid_spectral_terms=0 ELSE degrid_spectral_terms=Fix(degrid_spectral_terms)
IF N_Elements(grid_spectral_terms) EQ 0 THEN grid_spectral_terms=0 ELSE grid_spectral_terms=Fix(grid_spectral_terms)
IF N_Elements(spectral_index) EQ 0 THEN IF Keyword_Set(degrid_spectral_terms) THEN spectral_index=0. ELSE spectral_index=-0.8 
IF N_Elements(dft_threshold) EQ 0 THEN dft_threshold=0. 
IF dft_threshold EQ 1 THEN dft_threshold=1./((2.*!Pi)^2.*dimension)
IF N_Elements(nside) EQ 0 THEN nside=0
IF N_Elements(restrict_hpx_inds) NE 1 THEN ind_list="UNSPECIFIED" ELSE ind_list=restrict_hpx_inds
IF N_Elements(n_hpx) EQ 0 THEN n_hpx=0
IF N_Elements(n_zero_hpx) EQ 0 THEN n_zero_hpx=-1
IF Keyword_Set(double_precision) THEN double_precision=1 ELSE double_precision=0
IF dimension GT 4096 THEN BEGIN
    IF double_precision EQ 0 THEN BEGIN
        print, "WARNING: If dimension is greater than 4096 you MUST use double precision!"
        print, "Turning on double precision"
        double_precision=1
    ENDIF
ENDIF
pol_names=['XX','YY','XY','YX','I','Q','U','V']
healpix={nside:Long(nside),ind_list:String(ind_list),n_pix:Long(n_hpx),n_zero:Long(n_zero_hpx)}

arr={tile_A:Long(tile_A),tile_B:Long(tile_B),bin_offset:Long(bin_offset),Jdate:meta.Jdate,freq:Double(frequency_array),fbin_i:Long(freq_bin_i),$
    freq_use:Fix(freq_use),tile_use:Fix(tile_use),time_use:Fix(time_use),tile_names:String(meta.tile_names),tile_height:Float(meta.tile_height),tile_flag:meta.tile_flag}
struct={code_version:String(code_version),instrument:String(instrument),obsname:String(obsname),$
    dimension:Float(dimension),elements:Float(elements),nbaselines:Long(nbaselines),dft_threshold:Float(dft_threshold),double_precision:double_precision,$
    kpix:Float(kbinsize),degpix:Float(degpix),obsaz:meta.obsaz,obsalt:meta.obsalt,obsra:meta.obsra,obsdec:meta.obsdec,$
    zenra:meta.zenra,zendec:meta.zendec,obsx:meta.obsx,obsy:meta.obsy,zenx:meta.zenx,zeny:meta.zeny,$
    phasera:meta.phasera,phasedec:meta.phasedec,orig_phasera:meta.orig_phasera,orig_phasedec:meta.orig_phasedec,$
    n_pol:Fix(n_pol,type=2),n_tile:Long(n_tile),n_tile_flag:Long(n_flag),n_freq:Long(n_freq),n_freq_flag:0L,n_time:Long(n_time),n_time_flag:n_time_cut,$
    n_vis:Long(n_vis),n_vis_in:Long(n_vis_in),n_vis_raw:Long(n_vis_raw),nf_vis:Long(n_vis_arr),primary_beam_area:Ptrarr(4),primary_beam_sq_area:Ptrarr(4),pol_names:pol_names,$
    jd0:meta.jd0,max_baseline:Double(max_baseline),min_baseline:Double(min_baseline),delays:meta.delays,lon:meta.lon,lat:meta.lat,alt:meta.alt,$
    freq_center:Float(freq_center),freq_res:Float(freq_res),time_res:Float(meta.time_res),astr:meta.astr,alpha:Float(spectral_index),$
    residual:0,vis_noise:noise_arr,baseline_info:Ptr_new(arr),meta_data:meta_data,meta_hdr:meta_hdr,$
    degrid_spectral_terms:degrid_spectral_terms,grid_spectral_terms:grid_spectral_terms,grid_info:grid_info,healpix:healpix}    
RETURN,struct
END
