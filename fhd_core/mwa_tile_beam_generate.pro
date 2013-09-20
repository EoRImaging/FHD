FUNCTION mwa_tile_beam_generate,antenna_gain_arr,antenna_beam_arr,obsaz=obsaz,obsza=obsza,$
    frequency=frequency,polarization=polarization,$
    psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
    normalization=normalization,xvals=xvals,yvals=yvals,$
    dimension=dimension,elements=elements,za_arr=za_arr,az_arr=az_arr,delay_settings=delay_settings,_Extra=extra

compile_opt idl2,strictarrsubs  
;indices of antenna_gain_arr correspond to these antenna locations:
;         N
;    0  1  2  3
;    
;    4  5  6  7  
;W                E
;    8  9  10 11   
;    
;    12 13 14 15 
;         S
;polarization 0: x, 1: y
;angle offset is the rotation of the entire tile in current coordinates in DEGREES
; (this should be the rotation between E-W or N-S and Ra-Dec)

IF N_Elements(normalization) EQ 0 THEN normalization=1.
psf_dim=Float(psf_dim)
psf_dim2=dimension

;kbinsize_use=kbinsize;/psf_resolution
;degpix_use=!RaDeg/(kbinsize_use*psf_dim2) 
;IF N_Elements(xvals) EQ 0 THEN xvals=(meshgrid(psf_dim2,psf_dim2,1)-psf_dim2/2.)*degpix_use
;IF N_Elements(yvals) EQ 0 THEN yvals=(meshgrid(psf_dim2,psf_dim2,2)-psf_dim2/2.)*degpix_use
;IF (N_Elements(obsaz)EQ 0) OR (N_Elements(obsza) EQ 0) THEN BEGIN
;    x1=reform(xvals[*,psf_dim2/2.]) & x1i=where(x1)
;    y1=reform(yvals[psf_dim2/2.,*]) & y1i=where(y1)
;    x0=interpol(x1i,x1[x1i],0.)
;    y0=interpol(y1i,y1[y1i],0.)
;    za=Interpolate(za_arr,x0,y0,cubic=-0.5)
;    az=Interpolate(az_arr,x0,y0,cubic=-0.5)
;ENDIF ELSE BEGIN
    za=obsza
    az=obsaz
;ENDELSE

antenna_spacing=1.1 ;meters (Same as M&C SetDelays script) ; Was 1.071 before?
antenna_length=29.125*2.54/100. ;meters (measured)
antenna_height=0.35 ;meters (rumor)
c_light_vacuum=299792458.
c_light_cable=210000000. ;not used

Kconv=(2.*!Pi)*(frequency/c_light_vacuum) ;wavenumber (radians/meter)
wavelength=c_light_vacuum/frequency

IF Keyword_Set(antenna_beam_arr) THEN IF Keyword_Set(*antenna_beam_arr[0]) THEN BEGIN
    tile_beam=fltarr(psf_dim2,psf_dim2)
    FOR i=0,15 DO tile_beam+=*antenna_beam_arr[i]*antenna_gain_arr[i]
    tile_beam*=normalization
    tile_beam=tile_beam
    RETURN,tile_beam
ENDIF
xc_arr0=Reform((meshgrid(4,4,1))*antenna_spacing,16)
xc_arr=xc_arr0-Mean(xc_arr0) ;dipole east position (meters)
yc_arr0=Reform(Reverse(meshgrid(4,4,2),2)*antenna_spacing,16)
yc_arr=yc_arr0-Mean(yc_arr0) ;dipole north position (meters)
zc_arr=Fltarr(16)

;term_A=Tan(az*!DtoR)
;term_B=za*!DtoR
;xc=Sqrt((term_B^2.)/(1+term_A^2.))
;yc=term_A*xc
za_arr_use=Reform(za_arr,(psf_dim2)^2.)
az_arr_use=Reform(az_arr,(psf_dim2)^2.)

;beamformer phase setting (meters) 
IF not Ptr_valid(delay_settings) THEN BEGIN
    D0_d=xc_arr0*sin(za*!DtoR)*Sin(az*!DtoR)+yc_arr0*Sin(za*!DtoR)*Cos(az*!DtoR) 
    D0_d/=c_light_vacuum*4.35E-10 ;435 picoseconds is base delay length unit
    delay_settings=Ptr_new(Round(D0_d)) ;round to nearest real delay setting
ENDIF
D0_d=*delay_settings*c_light_vacuum*4.35E-10
D0_d=Float(D0_d)
D0_d-=Min(D0_d)

;proj_east=Reform(xvals,(psf_dim2)^2.)
;proj_north=Reform(yvals,(psf_dim2)^2.)
;proj_z=Cos(za_arr_use*!DtoR)

proj_east=Sin(za_arr*!DtoR)*Sin(az_arr*!DtoR) & proj_east_use=Reform(proj_east,(psf_dim2)^2.)
proj_north=Sin(za_arr*!DtoR)*Cos(az_arr*!DtoR) & proj_north_use=Reform(proj_north,(psf_dim2)^2.)
proj_z=Cos(za_arr*!DtoR) & proj_z_use=Reform(proj_z,(psf_dim2)^2.)

;phase of each dipole for the source (relative to the beamformer settings)
;D_d=(proj_east#xc_arr+proj_north#yc_arr+proj_z#zc_arr-replicate(1,(psf_dim2)^2.)#D0_d*kbinsize_use);/Kconv
D_d=(proj_east_use#xc_arr+proj_north_use#yc_arr+proj_z_use#zc_arr-replicate(1,(psf_dim2)^2.)#D0_d);/Kconv
D_d=Reform(D_d,psf_dim2,psf_dim2,16)

;groundplane=2.*Sin(Cos(za_arr_use*!DtoR)#(Kconv*(antenna_height+zc_arr))) ;looks correct
;groundplane=Reform(groundplane,psf_dim2,psf_dim2,16)

groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength)) ;should technically have zc_arr, but until that is nonzero this is the same and faster
;groundplane0=2.*Sin(2.*!Pi*antenna_height/wavelength) ;normalization factor
groundplane0=2.*Sin(Cos(za*!DtoR)*2.*!Pi*antenna_height/wavelength) ;normalization factor

IF polarization EQ 0 THEN projection=Sqrt(1.-proj_east^2.) ELSE projection=Sqrt(1.-proj_north^2.) 
;IF polarization EQ 0 THEN projection0=Sqrt(1.-(Sin(za*!DtoR)*Sin(az*!DtoR))^2.) $
;    ELSE projection0=Sqrt(1.-(Sin(za*!DtoR)*Cos(az*!DtoR))^2.) 
;projection/=projection0

;projection=Sqrt(projection)
;projection=1.

ii=Complex(0,1)

dipole_gain_arr=Exp(-ii*Kconv*D_d)
;horizon_test=where(abs(za_arr_use) GE 90.,n_horizon_test)
;horizon_mask=fltarr(psf_dim2,psf_dim2)+1
;IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    

IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(16,/allocate)
FOR i=0,15 DO *antenna_beam_arr[i]=dipole_gain_arr[*,*,i]*groundplane*projection/(16.*groundplane0);*pol

tile_beam=fltarr(psf_dim2,psf_dim2)
FOR i=0,15 DO tile_beam+=*antenna_beam_arr[i]*antenna_gain_arr[i]

tile_beam*=normalization;*horizon_mask;*uv_mask

;tile_beam=tile_beam>0.
RETURN,tile_beam

END