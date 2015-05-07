FUNCTION generate_source_cal_list,obs,psf,catalog_path=catalog_path,calibration_spectral_index=calibration_spectral_index,$
    max_calibration_sources=max_calibration_sources,calibration_flux_threshold=calibration_flux_threshold,$
    no_restrict_cal_sources=no_restrict_cal_sources,no_extend=no_extend,mask=mask,beam_cal_threshold=beam_cal_threshold,$
    allow_sidelobe_cal_sources=allow_sidelobe_cal_sources,beam_arr=beam_arr,model_visibilities=model_visibilities,$
    max_model_sources=max_model_sources,model_flux_threshold=model_flux_threshold,delicate_calibration_catalog=delicate_calibration_catalog,$
    no_restrict_model_sources=no_restrict_model_sources,model_spectral_index=model_spectral_index,$
    allow_sidelobe_model_sources=allow_sidelobe_model_sources,beam_model_threshold=beam_model_threshold,flatten_spectrum=flatten_spectrum,_Extra=extra

UPNAME=StrUpCase(catalog_path)
psav=strpos(UPNAME,'.SAV')>strpos(UPNAME,'.IDLSAVE')
IF psav EQ -1 THEN catalog_path+='.sav'
IF file_test(catalog_path) EQ 0 THEN BEGIN
    catalog_path_full=filepath(catalog_path,root=Rootdir('fhd'),subdir='catalog_data')
    IF file_test(catalog_path_full) EQ 0 THEN BEGIN
        if keyword_set(delicate_calibration_catalog) then begin
          print,String(format='(A," not found! Critical problem, quitting!',catalog_path)
          exit
        endif else begin
          print,String(format='(A," not found! Using default: ",A)',catalog_path,obs.instrument+'_calibration_source_list.sav')
          catalog_path=obs.instrument+'_calibration_source_list.sav'
          catalog_path_full=filepath(catalog_path,root=Rootdir('fhd'),subdir='catalog_data')
        endelse
    ENDIF
ENDIF ELSE catalog_path_full=catalog_path

cat_init=source_comp_init(n_sources=0) ;define structure BEFORE restoring, in case the definition has changed
RESTORE,catalog_path_full,/relaxed ;catalog

IF Keyword_Set(model_visibilities) THEN BEGIN
    IF N_Elements(model_flux_threshold) GT 0 THEN flux_threshold=model_flux_threshold ELSE flux_threshold=0.
    IF N_Elements(allow_sidelobe_model_sources) GT 0 THEN allow_sidelobe_sources=allow_sidelobe_model_sources ELSE allow_sidelobe_sources=0
    IF Keyword_Set(allow_sidelobe_sources) THEN IF N_Elements(no_restrict_model_sources) EQ 0 THEN no_restrict_sources=1 ELSE no_restrict_sources=no_restrict_model_sources $
        ELSE no_restrict_sources=1
    IF N_Elements(max_model_sources) GT 0 THEN max_sources=max_model_sources ELSE max_sources=0 ;0 turns it off
    IF N_Elements(model_spectral_index) GT 0 THEN spectral_index=model_spectral_index ELSE spectral_index=catalog.alpha
    IF N_Elements(beam_model_threshold) GT 0 THEN beam_threshold=beam_model_threshold
ENDIF ELSE BEGIN
    IF N_Elements(calibration_flux_threshold) GT 0 THEN flux_threshold=calibration_flux_threshold ELSE flux_threshold=0.
    IF N_Elements(allow_sidelobe_cal_sources) GT 0 THEN allow_sidelobe_sources=allow_sidelobe_cal_sources ELSE allow_sidelobe_sources=0
    IF Keyword_Set(allow_sidelobe_sources) THEN IF N_Elements(no_restrict_cal_sources) EQ 0 THEN no_restrict_sources=1 ELSE no_restrict_sources=no_restrict_cal_sources $
        ELSE no_restrict_sources=1
    IF N_Elements(max_calibration_sources) GT 0 THEN max_sources=max_calibration_sources ELSE max_sources=0 ;0 turns it off
    IF N_Elements(calibration_spectral_index) GT 0 THEN spectral_index=calibration_spectral_index ELSE spectral_index=catalog.alpha
    IF N_Elements(beam_cal_threshold) GT 0 THEN beam_threshold=beam_cal_threshold
ENDELSE
    
astr=obs.astr
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

FoV=!RaDeg/obs.kpix
freq_arr=psf.freq
freq_use=obs.freq_center
IF freq_use GT 1E5 THEN freq_use/=1E6
n_pol=obs.n_pol

ra0=astr.crval[0]
dec0=astr.crval[1]
angs=angle_difference(dec0,ra0,catalog.dec,catalog.ra,/degree)
i_use=where(Abs(angs) LE FoV/2.,n_use)

IF Keyword_Set(no_restrict_sources) THEN BEGIN
    fft_alias_range=0.
    IF N_Elements(beam_threshold) EQ 0 THEN $
        IF Keyword_Set(allow_sidelobe_sources) THEN beam_threshold=0.01 ELSE beam_threshold=0.05
ENDIF ELSE BEGIN
    fft_alias_range=dimension/4.
    IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.2
ENDELSE

IF n_use GT 0 THEN BEGIN
    catalog=catalog[i_use]
    IF N_Elements(spectral_index) GT 1 THEN spectral_index=spectral_index[i_use]
    source_list=source_comp_init(n_sources=n_use,freq=freq_use,ra=catalog.ra,dec=catalog.dec,$
        alpha=spectral_index,extend=catalog.extend)
        
    ad2xy,source_list.ra,source_list.dec,astr,x_arr,y_arr
    source_list.x=x_arr
    source_list.y=y_arr
;    IF N_Elements(calibration_spectral_index) EQ 0 THEN calibration_spectral_index=catalog.alpha
    FOR i=0,7 DO source_list.flux.(i)=catalog.flux.(i)*(freq_use/catalog.freq)^spectral_index
    source_list.alpha=spectral_index
;    source_list.StoN=catalog.StoN
    
    IF N_Elements(beam_arr) LT (n_pol<2) THEN BEGIN 
        beam_arr=Ptrarr(n_pol<2)
        FOR pol_i=0,(n_pol<2)-1 DO beam_arr[pol_i]=Ptr_new(beam_image(psf,obs,pol_i=pol_i,square=0)>0.)
    ENDIF
    beam=fltarr(dimension,elements)
    FOR pol_i=0,(n_pol<2)-1 DO beam+=*beam_arr[pol_i]^2.
    beam=Sqrt(beam/(n_pol<2))

    IF Keyword_Set(allow_sidelobe_sources) THEN beam_i=where(beam GT beam_threshold) $
        ELSE beam_i=region_grow(beam,dimension/2.+dimension*elements/2.,threshold=[Max(beam)/2.<beam_threshold,Max(beam)>1.])
    beam_mask=fltarr(dimension,elements) & beam_mask[beam_i]=1.
    IF N_Elements(mask) EQ N_Elements(beam_mask) THEN beam_mask*=mask

    src_use=where((x_arr GE fft_alias_range) AND (x_arr LE dimension-1-fft_alias_range) AND (y_arr GE fft_alias_range) $
        AND (y_arr LE elements-1-fft_alias_range) AND (source_list.flux.I GT flux_threshold),n_src_use)
    
    IF n_src_use EQ 0 THEN RETURN,source_comp_init(n_sources=0,freq=obs.freq_center);
    src_use2=where(beam_mask[Round(x_arr[src_use]),Round(y_arr[src_use])],n_src_use)
    IF n_src_use GT 0 THEN src_use=src_use[src_use2]
    source_list=source_list[src_use]
    beam_list=Ptrarr(n_pol<2)
    
    influence=source_list.flux.I*beam[source_list.x,source_list.y]
    
    order=Reverse(sort(influence))
    source_list=source_list[order]
    source_list.id=Lindgen(n_src_use)
    IF Keyword_Set(no_extend) THEN source_list.extend=Ptrarr(n_src_use) ELSE BEGIN
        extend_i=where(Ptr_valid(source_list.extend),n_extend)
        FOR ext_i=0L,n_extend-1 DO BEGIN
            ex_spectral_index=source_list[extend_i[ext_i]].alpha
            extend_list=*source_list[extend_i[ext_i]].extend
            ad2xy,extend_list.ra,extend_list.dec,astr,x_arr,y_arr
            extend_list.x=x_arr
            extend_list.y=y_arr
            extend_list.alpha=ex_spectral_index
            FOR i=0,7 DO extend_list.flux.(i)=extend_list.flux.(i)*(freq_use/catalog.freq)^ex_spectral_index
;            FOR pol_i=0,(n_pol<2)-1 DO extend_list.flux.(pol_i)=extend_list.flux.I*(*beam_list[pol_i])[extend_i[ext_i]]
            *source_list[extend_i[ext_i]].extend=extend_list
        ENDFOR
    ENDELSE
ENDIF ELSE RETURN,source_comp_init(n_sources=0,freq=obs.freq_center)

IF Keyword_Set(max_sources) THEN IF N_Elements(source_list) GT max_sources $
    THEN source_list=source_list[0:max_sources-1]

IF Keyword_Set(flatten_spectrum) THEN BEGIN
    alpha_avg=Total(source_list.alpha*(source_list.flux.I>0))/Total(source_list.flux.I>0)
    obs.alpha=alpha_avg
ENDIF
RETURN,source_list
END