FUNCTION observation_healpix_inds_select,obs,ra_use=ra_use,dec_use=dec_use,freq=freq
str_base={name:'',ra:0.,dec:0.,freq:0.}
IF Keyword_Set(obs) THEN BEGIN
    freq_use=obs.freq_center/1E6 ;MHz
    ra_use=obs.obsra
    dec_use=obs.obsdec
    ang_tolerance=1. ;degrees
ENDIF ELSE BEGIN
    IF freq GT 1E6 THEN freq_use=freq/1E6 ELSE freq_use=freq
    ang_tolerance=1. ;degrees
ENDELSE

n_select=4
str=Replicate(str_base,n_select)
str[0]={name:'EoR0_high_healpix_inds.idlsave',ra:0.,dec:-30.,freq:182.}
str[1]={name:'EoR0_low_healpix_inds.idlsave',ra:0.,dec:-30.,freq:151.}
str[2]={name:'EoR1_high_healpix_inds.idlsave',ra:60.,dec:-30.,freq:182.}
str[3]={name:'EoR1_low_healpix_inds.idlsave',ra:60.,dec:-30.,freq:151.}


ang_dist=Abs(angle_difference(dec_use,ra_use,str.dec,str.ra,/degree,/nearest))
ang_use=Min(ang_dist)
i_use=where(Abs(ang_dist-ang_use) LE ang_tolerance,n_use)
str=str[i_use]
freq_dist=Abs(str.freq-freq_use)
f_use=Min(freq_dist,min_i)
restrict_hpx_inds=str[min_i].name

RETURN,restrict_hpx_inds
END