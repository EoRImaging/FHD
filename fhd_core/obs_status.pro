PRO obs_status,obs

obs_info=*(obs.baseline_info)
kbinsize=obs.kpix
degpix=obs.degpix
dimension=obs.dimension
fov=dimension*degpix
k_span=kbinsize*dimension
freq_arr=obs_info.freq
freq_use=obs_info.freq_use
bw_start=Min(freq_arr[where(freq_use)])
bw_end=Max(freq_arr[where(freq_use)])
bandwidth=Round((bw_end-bw_start)/1E5)/10.

print,String(format='("Image size used: ",A," pixels")',Strn(dimension))
print,String(format='("Image resolution used: ",A," degrees/pixel")',Strn(degpix))
print,String(format='("Approx. beam area: ",A," pixels")',Strn((!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)))
print,String(format='("Field of view used: ",A," degrees")',Strn(fov))
print,String(format='("Frequency range: ",A,"-",A," MHz")',Strn(Round((bw_start)/1E5)/10.),Strn(Round((bw_end)/1E5)/10.))
print,String(format='("UV resolution used: ",A," wavelengths/pixel")',Strn(kbinsize))
print,String(format='("UV image size used: ",A," wavelengths")',Strn(k_span))
print,String(format='("Min baseline: ",A," wavelengths")',Strn(obs.min_baseline))
print,String(format='("Max baseline: ",A," wavelengths")',Strn(obs.max_baseline))
print,String(format='("Observation pointing (Az,El): ",A," ",A)',$
    Strn(obs.obsaz,length=6),Strn(obs.obsalt,length=5))
IF Ptr_valid(obs.delays) THEN print,String(format='("Beamformer settings: ",(I))',*obs.delays)
print,String(format='("Observation coordinates: ",A," ",A,A)',$
    Strn(obs.obsra,length=7),(obs.obsdec GE 0) ? '+':'-',Strn(Abs(obs.obsdec),length=6))
print,String(format='("Zenith coordinates: ",A," ",A,A)',$
    Strn(obs.zenra,length=7),(obs.zendec GE 0) ? '+':'-',Strn(Abs(obs.zendec),length=6))
IF (obs.phasera NE obs.obsra) OR (obs.phasedec NE obs.obsdec) THEN $
    print,String(format='("Image phased to coordinates: ",A," ",A,A)',$
        Strn(obs.phasera,length=7),(obs.phasedec GE 0) ? '+':'-',Strn(Abs(obs.phasedec),length=6))
IF Tag_exist(obs,'alpha') THEN print,String(format='("Spectral index fit: ",A)',Strn(obs.alpha))
END