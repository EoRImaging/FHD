FUNCTION fhd_diffuse_model,obs,jones

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
n_pol=obs.n_pol
xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr

freq_use=where((*obs.baseline_info).freq_use,nf_use)
f_bin=(*obs.baseline_info).fbin_i
fb_use=Uniq(f_bin[freq_use])
nbin=N_Elements(fb_use)
freq_arr=(*obs.baseline_info).freq
alpha=obs.alpha
freq_norm=freq_arr^(-alpha)
;freq_norm/=Sqrt(Mean(freq_norm^2.))
freq_norm/=Mean(freq_norm) 
d_freq=Median(Float(deriv(freq_arr)))

freq_norm=freq_norm[freq_use[fb_use]]

freq_arr_use=freq_arr[freq_use[fb_use]]/1E6
fb_hist=histogram(f_bin[freq_use],min=0,bin=1)
nf_arr=fb_hist[f_bin[freq_use[fb_use]]]


RETURN,model_arr
END