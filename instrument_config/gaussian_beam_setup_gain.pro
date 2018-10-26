FUNCTION gaussian_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,Jdate_use=Jdate_use

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
n_tile=obs.n_tile
xvals_interp=za_arr*Sin(az_arr*!DtoR)/obs.degpix+obs.dimension/2.
yvals_interp=za_arr*Cos(az_arr*!DtoR)/obs.degpix+obs.elements/2.
freq_center=antenna[0].freq ;all need to be identical, so just use the first
;speed_light=299792458. ;speed of light, in meters/second
;icomp=Complex(0,1)

;calculate group identifications (used to set pointers to identical models)
FOR pol_i=0,n_ant_pol-1 DO BEGIN
    gi=0
    n_ungrouped=n_tile
    ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
    WHILE n_ungrouped GT 0 DO BEGIN
        ref_i=ungrouped_i[0]
        antenna[ref_i].group_id[pol_i]=gi
        FOR ug_i=1L,n_ungrouped-1 DO IF Total(*antenna[ungrouped_i[ug_i]].gain[pol_i] - *antenna[ref_i].gain[pol_i]) EQ 0 THEN antenna[ungrouped_i[ug_i]].group_id[pol_i]=gi 
        ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
        gi+=1
    ENDWHILE
ENDFOR

;Setup horizon mask and initialize Jones
horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
horizon_mask=fltarr(psf_image_dim,psf_image_dim)+1
IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0 
Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)


beam_model_version = Max(antenna.model_version)  ; Determines which FWHM to use.
print, 'beam_model_version in gaussian beam generator: ', beam_model_version
case beam_model_version of
    0: sigma = 100./(2*sqrt(2.*alog(2.)))
    1: sigma = 70./(2*sqrt(2.*alog(2.)))
    2: sigma = 40./(2*sqrt(2.*alog(2.)))
    3: sigma = 35./(2*sqrt(2.*alog(2.)))
    4: sigma = 16./(2*sqrt(2.*alog(2.)))
    5: sigma = 10./(2*sqrt(2.*alog(2.)))
END
print, 'hwhm = ', sigma*2.355/2., ' model: ', beam_model_version
beam_vals = Exp(-za_arr^2./(2.*sigma^2.));/(2 * sigma * sqrt(2*!pi))

FOR pol_i=0,1 DO BEGIN
    ;FOR freq_i=0,nfreq_bin-1 DO Jones_matrix[pol_i,pol_i,freq_i]=Ptr_new(Interpolate(beam_vals, xvals_interp, yvals_interp)*horizon_mask)
    FOR freq_i=0,nfreq_bin-1 DO Jones_matrix[pol_i,pol_i,freq_i]=Ptr_new(beam_vals*horizon_mask)
    FOR freq_i=0,nfreq_bin-1 DO Jones_matrix[pol_i,(pol_i+1) mod 2,freq_i]=Ptr_new(Fltarr(psf_image_dim,psf_image_dim))
ENDFOR
antenna.jones=Jones_matrix

RETURN,antenna
END
