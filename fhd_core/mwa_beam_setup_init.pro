FUNCTION mwa_beam_setup_init,obs,file_path_fhd,dipole_mutual_coupling_factor=dipole_mutual_coupling_factor,$
    beam_model_version=beam_model_version,za_arr=za_arr,az_arr=az_arr

;indices of gain_arr correspond to these antenna locations
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

n_tiles=obs.n_tile
n_freq=obs.n_freq
n_dipoles=16
antenna_spacing=1.1 ;meters (Same as M&C SetDelays script) ; Was 1.071 before? Verified in Tingay et al 2013
antenna_length=29.125*2.54/100. ;meters (measured) (NOT USED)
antenna_height=0.29 ;meters (June 2014 e-mail from Brian Crosse) ; Was 0.35 before
velocity_factor=0.673
speed_light=299792458. ;speed of light, in meters/second

obsra=obs.obsra
obsdec=obs.obsdec
zenra=obs.zenra
zendec=obs.zendec
phasera=obs.phasera
phasedec=obs.phasedec
Jdate=obs.Jd0
frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
bin_offset=(*obs.baseline_info).bin_offset
nbaselines=bin_offset[1]

dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
degpix=obs.degpix
astr=obs.astr
delay_settings=obs.delays
;IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.

;IF Keyword_Set(swap_pol) THEN pol_arr=[[1,1],[0,0],[1,0],[0,1]] ELSE pol_arr=[[0,0],[1,1],[0,1],[1,0]] 
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=16. ;=32?
IF N_Elements(psf_image_resolution) EQ 0 THEN psf_image_resolution=10.
Eq2Hor,obsra,obsdec,Jdate,obsalt,obsaz,lat=obs.lat,lon=obs.lon,alt=obs.alt
obsalt=Float(obsalt)
obsaz=Float(obsaz)
obsza=90.-obsalt
psf_dim=Ceil((obs.antenna_size*2.*Max(frequency_array)/speed_light)/kbinsize/Cos(obsza*!DtoR))  
psf_dim=Ceil(psf_dim/2.)*2. ;dimension MUST be even

freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO BEGIN
    fi_i=where(freq_bin_i EQ fi,n_fi)
    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) $
        ELSE freq_center[fi]=Median(frequency_array[fi_i])
ENDFOR


t1_a=Systime(1)
;set up coordinates to generate the high uv resolution model. 
;Remember that field of view = uv resolution, image pixel scale = uv span. 
;So, the cropped uv span (psf_dim) means we do not need to calculate at full image resolution, 
;   while the increased uv resolution can correspond to super-horizon scales. We construct the beam model in image space, 
;   and while we don't need the full image resolution we need to avoid quantization errors that come in if we make too small an image and then take the FFT
psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model
psf_superres_dim=psf_dim*psf_resolution
psf_scale=dimension*psf_intermediate_res/psf_image_dim
xvals_celestial=meshgrid(psf_image_dim,psf_image_dim,1)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
yvals_celestial=meshgrid(psf_image_dim,psf_image_dim,2)*psf_scale-psf_image_dim*psf_scale/2.+dimension/2.
xvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,1)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
yvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,2)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)

xy2ad,xvals_celestial,yvals_celestial,astr,ra_arr,dec_arr
valid_i=where(Finite(ra_arr),n_valid)
ra_use=ra_arr[valid_i]
dec_use=dec_arr[valid_i]

;NOTE: Eq2Hor REQUIRES Jdate to have the same number of elements as RA and Dec for precession!!
;;NOTE: The NEW Eq2Hor REQUIRES Jdate to be a scalar! They created a new bug when they fixed the old one
Eq2Hor,ra_use,dec_use,Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
za_arr=fltarr(psf_image_dim,psf_image_dim)+90. & za_arr[valid_i]=90.-alt_arr1
az_arr=fltarr(psf_image_dim,psf_image_dim) & az_arr[valid_i]=az_arr1

xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)

xc_arr0=Reform((meshgrid(4,4,1))*antenna_spacing,16)
xc_arr=xc_arr0-Mean(xc_arr0) ;dipole east position (meters)
yc_arr0=Reform(Reverse(meshgrid(4,4,2),2)*antenna_spacing,16)
yc_arr=yc_arr0-Mean(yc_arr0) ;dipole north position (meters)
zc_arr=Fltarr(16)
antenna_coords=Fltarr(n_dipoles,3)
antenna_coords[0]=xc_arr
antenna_coords[1]=yc_arr
antenna_coords[2]=zc_arr

base_gain=fltarr(16)+1.
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''

gain_arr=Ptrarr(2)
FOR pol_i=0,1 DO gain_arr[pol_i]=Ptr_new(Rebin(reform(base_gain,1,1,n_dipoles),nfreq_bin,n_tiles,n_dipoles,/sample))

IF file_test(file_path_fhd+'_dipole_gains.sav') THEN restore,file_path_fhd+'_dipole_gains.sav'
IF Keyword_Set(dipole_mutual_coupling_factor) THEN mutual_coupling=mwa_dipole_mutual_coupling(freq_center) ELSE mutual_coupling=Ptr_new() 

CASE beam_model_version OF
    2: BEGIN
        file_path_J_matrix=filepath('Jmatrix.fits',root=rootdir('FHD'),sub='instrument_config')
        fits_info,file_path_J_matrix,/silent,n_ext=n_ext
        ;Cols of J matrix: theta phi  real(Jxt(t,p)) imag(Jxt(t,p)) real(Jxp(t,p)) imag(Jxp(t,p)) real(Jyt(t,p)) imag(Jyt(t,p)) real(Jyp(t,p)) imag(Jyp(t,p)))
        ;Where theta is the zenith angle, phi is angle measured clockwise from +east direction looking down
        ; Jxt is the Jones mapping unit vec in theta (t) direction to the x (east-west) dipole etc
        n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
        freq_arr_Jmat=Fltarr(n_ext)
        
        FOR ext_i=0,n_ext-1 DO BEGIN
            Jmat1=mrdfits(file_path_J_matrix,ext_i,header,status=status,/silent)
            IF ext_i EQ 0 THEN BEGIN
                n_pol_param=Float(sxpar(header,'NAXIS1'))
                n_pol_param=8/4
                n_ang=Float(sxpar(header,'NAXIS2'))
                Jmat_arr=Complexarr(n_ext,n_pol_param,n_pol_param,n_ang)
                theta_arr=Fltarr(n_ext,n_ang)
                phi_arr=Fltarr(n_ext,n_ang)
            ENDIF
            theta_arr[ext_i,*]=Jmat1[0,*] ;zenith angle in degrees
            phi_arr[ext_i,*]=Jmat1[1,*] ;azimuth angle in degrees, clockwise from East
            FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat_arr[ext_i,p_i,p_j,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
            freq_arr_Jmat[ext_i]=Float(sxpar(header,'FREQ')) ;in Hz
        ENDFOR
        theta_arr=median(theta_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=median(phi_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=270.-phi_arr ;change azimuth convention
        
    ;    Jmat_return=Ptrarr(n_pol_param,n_pol_param)
        Jmat_interp=Ptrarr(n_pol_param,n_pol_param)
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat_interp[p_i,p_j]=Ptr_new(Complexarr(n_ang))
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO FOR a_i=0L,n_ang-1 DO $
            (*Jmat_interp[p_i,p_j])[a_i]=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,frequency)
        
        xv_model=theta_arr*Sin(phi_arr*!DtoR)
        yv_model=theta_arr*Cos(phi_arr*!DtoR)
        
        horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
        horizon_mask=fltarr(psf_dim2,psf_dim2)+1
        IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    
        Jmat=Ptrarr(n_pol_param,n_pol_param)
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat[p_i,p_j]=Ptr_new(Complexarr(psf_dim2,psf_dim2))
        FOR i=0L,n_pix-1 DO BEGIN
            xv_instrument1=xvals[pix_use[i]]
            yv_instrument1=yvals[pix_use[i]]
            
            dist_test=Sqrt((xv_instrument1-xv_model)^2.+(yv_instrument1-yv_model)^2.)
            IF Min(dist_test,min_i) EQ 0 THEN BEGIN ;test if there is an exact match. Then just take that value and do no interpolation
                FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO (*Jmat[p_i,p_j])[pix_use[i]]=(*Jmat_interp[p_i,p_j])[min_i]
                CONTINUE ;move on to next iteration of pixel FOR loop
            ENDIF 
            i_use=(Sort(dist_test))[0:3] ;there had better be at least four points in the model!
            weight=1./dist_test[i_use]
            FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO (*Jmat[p_i,p_j])[pix_use[i]]=Total((*Jmat_interp[p_i,p_j])[i_use]*weight)/Total(weight)
        ENDFOR
        IF polarization EQ 0 THEN projection=Sqrt(*Jmat[0,0]*Conj(*Jmat[0,0])+*Jmat[1,0]*Conj(*Jmat[1,0])) $
            ELSE projection=Sqrt(*Jmat[1,1]*Conj(*Jmat[1,1])+*Jmat[0,1]*Conj(*Jmat[0,1]))
        projection/=Max(projection)
    END
    ELSE: BEGIN                
        IF polarization EQ 0 THEN projection=Sqrt(1.-proj_east^2.) ELSE projection=Sqrt(1.-proj_north^2.) 
        groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength)) ;should technically have zc_arr, but until that is nonzero this is the same and faster
        groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength) ;normalization factor
        groundplane/=groundplane0
    ENDELSE
ENDCASE

antenna=fhd_struct_init_antenna(beam_model_version=beam_model_version,antenna_spacing=antenna_spacing,$
    antenna_length=antenna_length,antenna_height=antenna_height,velocity_factor=velocity_factor,freq_arr=freq_arr,$
    Jones_matrix=Jones_matrix,mutual_coupling=mutual_coupling,gain_arr=gain_arr,phased_array_flag=phased_array_flag,antenna_coords=antenna_coords)
RETURN,antenna
END