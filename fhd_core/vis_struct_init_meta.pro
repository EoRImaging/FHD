FUNCTION vis_struct_init_meta,file_path_vis,hdr,params,lon=lon,lat=lat,alt=alt,_Extra=extra

IF N_Elements(lon) EQ 0 THEN lon=116.67081524 & lon=Float(lon);degrees
IF N_Elements(lat) EQ 0 THEN lat=-26.7033194 & lat=Float(lat);degrees
IF N_Elements(alt) EQ 0 THEN alt=377.83 & alt=Float(alt);altitude (meters)
metafits_ext='.metafits'
metafits_dir=file_dirname(file_path_vis)
metafits_name=file_basename(file_path_vis,'.uvfits',/fold_case)
metafits_name=file_basename(metafits_name,'_cal',/fold_case) ;sometimes "_cal" is present, sometimes not.
metafits_path=metafits_dir+path_sep()+metafits_name+metafits_ext

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
    Jdate=date_string_to_julian(date_string)
    
    zenra=LST
    zendec=lat
    epoch=date_conv(date_string)/1000.
    Precess,zenra,zendec,epoch,2000.
    
    
ENDIF ELSE BEGIN
    ;use hdr and params to guess metadata
    
ENDELSE

meta={obsra:obsra,obsdec:obsdec,zenra:zenra,zendec:zendec,phasera:phasera,phasedec:phasedec,names:tile_names}

RETURN,meta
END