FUNCTION fhd_struct_update_obs,obs, dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    n_pol=n_pol,max_baseline=max_baseline,min_baseline=min_baseline,FoV=FoV,$
    obsx=obsx,obsy=obsy,nfreq_avg=nfreq_avg,spectral_index=spectral_index,_Extra=extra

;updates the structure containing frequently needed parameters relating to the observation
IF N_Elements(spectral_index) EQ 0 THEN spectral_index=obs.alpha
IF N_Elements(n_pol) EQ 0 THEN n_pol=obs.n_pol
IF N_Elements(max_baseline) EQ 0 THEN max_baseline=obs.max_baseline
IF N_Elements(min_baseline) EQ 0 THEN min_baseline=obs.min_baseline

b_info=*(obs.baseline_info)
n_freq=obs.n_freq
IF N_Elements(nfreq_avg) EQ 0 THEN nfreq_avg=Round(n_freq/(Max(b_info.fbin_i)+1.))
frequency_array=b_info.freq
freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
nfreq_bin=N_Elements(freq_hist)
freq_bin_i=fltarr(n_freq)
FOR bin=0L,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
freq_center=Median(frequency_array)

IF Keyword_Set(FoV) THEN kbinsize=!RaDeg/FoV
IF ~Keyword_Set(kbinsize) THEN kbinsize=0.5 ;k-space resolution, in wavelengths per pixel
IF N_Elements(degpix) EQ 0 THEN k_span=2.*max_baseline ELSE k_span=!RaDeg/degpix 
dimension_test=2.^Round(ALOG10(k_span/kbinsize)/ALOG10(2.))

max_baseline=max_baseline<(k_span/sqrt(2.))

IF N_Elements(dimension) EQ 0 THEN dimension=dimension_test ELSE dimension=Float(dimension);dimension of the image in pixels; dimension = x direction
IF N_Elements(elements) EQ 0 THEN elements=dimension ELSE elements=Float(elements);elements = y direction
degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel

struct=obs
struct.dimension=Float(dimension)
struct.elements=Float(elements)
struct.kpix=Float(kbinsize)
struct.degpix=Float(degpix)
struct.max_baseline=Float(max_baseline)
struct.min_baseline=Float(min_baseline)

struct.n_pol=Fix(n_pol,type=2)
struct.alpha=Float(spectral_index)

projection_slant_orthographic,astr=astr,degpix=degpix,obsra=obs.obsra,obsdec=obs.obsdec,zenra=obs.zenra,zendec=obs.zendec,$
    dimension=dimension,elements=elements,phasera=obs.phasera,phasedec=obs.phasedec,obsx=obsx,obsy=obsy,zenx=zenx,zeny=zeny,$
    epoch=2000.,JDate=obs.JD0,date_obs=obs.astr.dateobs


struct.obsx=Float(obsx)
struct.obsy=Float(obsy)
struct.zenx=Float(zenx)
struct.zeny=Float(zeny)
struct.astr=astr

RETURN,struct
END