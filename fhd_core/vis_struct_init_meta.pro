FUNCTION vis_struct_init_meta,file_path_vis,hdr,params,lon=lon,lat=lat,alt=alt,$
    zenra=zenra,zendec=zendec,obsra=obsra,obsdec=obsdec,phasera=phasera,phasedec=phasedec,$
    rephase_to_zenith=rephase_to_zenith,precess=precess,degpix=degpix,dimension=dimension,elements=elements,$
    obsx=obsx,obsy=obsy,mirror_X=mirror_X,mirror_Y=mirror_Y,_Extra=extra

IF N_Elements(lon) EQ 0 THEN lon=116.67081524 & lon=Float(lon);degrees
IF N_Elements(lat) EQ 0 THEN lat=-26.7033194 & lat=Float(lat);degrees
IF N_Elements(alt) EQ 0 THEN alt=377.83 & alt=Float(alt);altitude (meters)
metafits_ext='.metafits'
metafits_dir=file_dirname(file_path_vis)
metafits_name=file_basename(file_path_vis,'.uvfits',/fold_case)
metafits_name=file_basename(metafits_name,'_cal',/fold_case) ;sometimes "_cal" is present, sometimes not.
metafits_path=metafits_dir+path_sep()+metafits_name+metafits_ext

time=params.time
b0i=Uniq(time)
jdate=double(hdr.jd0)+time[b0i]

IF N_Elements(dimension) EQ 0 THEN dimension=1024.
IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(obsx) EQ 0 THEN obsx=dimension/2.
IF N_Elements(obsy) EQ 0 THEN obsy=elements/2.

degpix2=[degpix,degpix]
IF Keyword_Set(mirror_X) THEN degpix2[0]*=-1
IF Keyword_Set(mirror_Y) THEN degpix2[1]*=-1

IF file_test(metafits_path) THEN BEGIN
    hdr0=headfits(metafits_path,exten=0,/silent)
    
    data=mrdfits(metafits_path,1,hdr1,/silent)
    tile_names=data.tile
    obsra=sxpar(hdr0,'RA')
    obsdec=sxpar(hdr0,'Dec')
    phasera=sxpar(hdr0,'RAPHASE')
    phasedec=sxpar(hdr0,'DECPHASE')
    LST=sxpar(hdr0,'LST')
;    HA=sxpar(hdr0,'HA')
;    HA=ten([Fix(Strmid(HA,0,2)),Fix(Strmid(HA,3,2)),Fix(Strmid(HA,6,2))])*15.
    date_string=sxpar(hdr0,'DATE-OBS')
    JD0=date_string_to_julian(date_string)
    
    zenra=LST
    zendec=lat
    epoch=date_conv(date_string)/1000.
    Precess,zenra,zendec,epoch,2000.    
    
    beamformer_delays=sxpar(hdr0,'DELAYS')
    beamformer_delays=Ptr_new(Float(Strsplit(beamformer_delays,',',/extract)))
ENDIF ELSE BEGIN
    ;use hdr and params to guess metadata
    print,'### NOTE ###'
    print,'Metafits file not found! Calculating obs settings from the uvfits header instead'
    
;    print,metafits_path+' not found. Calculating obs settings from the uvfits header instead'
    ;256 tile upper limit is hard-coded in CASA format
    ;these tile numbers have been verified to be correct
    tile_A1=Long(Floor(params.baseline_arr/256)) ;tile numbers start from 1
    tile_B1=Long(Fix(params.baseline_arr mod 256))
    hist_A1=histogram(tile_A1,min=0,max=256,/binsize,reverse_ind=ria)
    hist_B1=histogram(tile_B1,min=0,max=256,/binsize,reverse_ind=rib)
    hist_AB=hist_A1+hist_B1
    tile_names=where(hist_AB,n_tile)
    
    year=Float(Strmid(hdr.date,0,4))
    month=Float(Strmid(hdr.date,5,2))
    day=Float(Strmid(hdr.date,8,2))
    epoch=Double(year+ymd2dn(year,month,day)/365.25)
    IF ~Keyword_Set(time_offset) THEN time_offset=0d
    time_offset/=(24.*3600.)
    JD0=Min(Jdate)+time_offset
    
    obsra=hdr.obsra
    obsdec=hdr.obsdec
    IF Keyword_Set(precess) THEN Precess,obsra,obsdec,epoch,2000.
    IF N_Elements(phasera) EQ 0 THEN phasera=obsra
    IF N_Elements(phasedec) EQ 0 THEN phasedec=obsdec
;    Precess,obsra,obsdec,2000.,epoch

    IF Keyword_Set(zenra) THEN BEGIN
        IF Keyword_Set(precess) THEN BEGIN
            IF N_Elements(zendec) EQ 0 THEN zendec=lat
            Precess,zenra,zendec,epoch,2000.
        ENDIF ELSE BEGIN
            IF N_Elements(zendec) EQ 0 THEN BEGIN
                zendec=lat
                zenra0=zenra
                Precess,zenra0,zendec,epoch,2000. ;slight error, since zenra0 is NOT in J2000, but assume the effect on zendec is small
            ENDIF
        ENDELSE
    ENDIF ELSE zenpos2,JD0,zenra,zendec, lat=lat, lng=lon,/degree,/J2000
    beamformer_delays=Ptr_new()
ENDELSE

IF Keyword_Set(rephase_to_zenith) THEN BEGIN
    phasera=obsra
    phasedec=obsdec
    obsra=zenra
    obsdec=zendec
ENDIF
projection_slant_orthographic,astr=astr,degpix=degpix2,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,obsx=obsx,obsy=obsy,zenx=zenx,zeny=zeny,phasera=phasera,phasedec=phasedec

meta={obsra:Float(obsra),obsdec:Float(obsdec),zenra:Float(zenra),zendec:Float(zendec),phasera:Float(phasera),phasedec:Float(phasedec),$
    epoch:Float(epoch),tile_names:tile_names,lon:Float(lon),lat:Float(lat),alt:Float(alt),JD0:Double(JD0),Jdate:Double(Jdate),astr:astr,$
    obsx:Float(obsx),obsy:Float(obsy),zenx:Float(zenx),zeny:Float(zeny),delays:beamformer_delays}

RETURN,meta
END