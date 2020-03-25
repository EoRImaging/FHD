FUNCTION hera_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,Jdate_use=Jdate_use

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
n_tile=obs.n_tile
beam_model_version=Max(antenna.model_version)
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

;build the instrumental pol Jones matrix
CASE beam_model_version OF
    ;Beams created by Dave Deboer in 2016
    2: BEGIN
    
        hera_beam_filepath_copol=filepath('HERA_beam_copol.sav',root=rootdir('FHD'),sub='instrument_config')
        hera_beam_filepath_crosspol=filepath('HERA_beam_crosspol.sav',root=rootdir('FHD'),sub='instrument_config')
        
        nside=getvar_savefile(hera_beam_filepath_copol,'nside')
        n_hpx=getvar_savefile(hera_beam_filepath_copol,'n_hpx')
        healpix_ordering=getvar_savefile(hera_beam_filepath_copol,'healpix_ordering')
        hera_frequency_array=getvar_savefile(hera_beam_filepath_copol,'hera_frequency_array')
        hera_beam_in=Ptrarr(2)
        hera_beam_in[0]=getvar_savefile(hera_beam_filepath_copol,'hera_beam_hpx',/pointer_return)
        hera_beam_in[1]=getvar_savefile(hera_beam_filepath_crosspol,'hera_beam_hpx',/pointer_return)
        
        hpx_inds=Lindgen(n_hpx)
        pix2vec_ring,nside,hpx_inds,pix_coords
        ;vec2ang,pix_coords,pix_za,pix_az ;returns RADIANS
       
        ;Get pixel values for the 90deg rotation 
        pix2ang_ring, nside, LINDGEN(n_hpx),theta,phi
        ang2pix_ring, nside, theta, phi+!pi/2., new_pix
        beam_pix = [[hpx_inds],[new_pix],[hpx_inds],[new_pix]]
        
        ;Create XX, YY, XY, and YX in that order
        matrix_inds = [[0,0],[1,1],[1,0],[0,1]]
        readin_inds = [0,0,1,1]
        FOR beam_i=0, 3 do begin
          
            ;Get a full Healpix interpolated beam for each frequency into a pointer
            hera_beam_interp=complex(Fltarr(n_hpx,nfreq_bin))
            FOR hpx_i=0L,n_hpx-1 DO hera_beam_interp[hpx_i,*]=Interpol((*hera_beam_in[readin_inds[beam_i]])[*,hpx_i],hera_frequency_array,freq_center)

            FOR freq_i=0,nfreq_bin-1 DO BEGIN

                ;Build up gridded beam for both real and complex values (healpix_interpolate is not complex)
                hera_beam_grid_arr = healpix_interpolate(real_part(hera_beam_interp[beam_pix[*,beam_i],freq_i]),obs,nside=nside,Jdate_use=Jdate_use,coord_sys='equatorial') $
                  + Complex(0,1) * healpix_interpolate(imaginary(hera_beam_interp[beam_pix[*,beam_i],freq_i]),obs,nside=nside,Jdate_use=Jdate_use,coord_sys='equatorial') 
                ;Create the Jones matrix
                ;Note: Elements of Jones matrix may not be in the proper basis to be used individually due to the 90deg rotation, though this will not affect the power beam
                Jones_matrix[matrix_inds[0,beam_i],matrix_inds[1,beam_i],freq_i]=Ptr_new((Interpolate(hera_beam_grid_arr,xvals_interp,yvals_interp)*horizon_mask)[pix_use])
            
            endfor
          
        ENDFOR  
    END
    ELSE: BEGIN

        hera_beam_filepath_X=filepath('HERA_beam_X.sav',root=rootdir('FHD'),sub='instrument_config')
        hera_beam_filepath_Y=filepath('HERA_beam_Y.sav',root=rootdir('FHD'),sub='instrument_config')
        
        nside=getvar_savefile(hera_beam_filepath_X,'nside')
        n_hpx=getvar_savefile(hera_beam_filepath_X,'n_hpx')
        healpix_ordering=getvar_savefile(hera_beam_filepath_X,'healpix_ordering')
        hera_frequency_array=getvar_savefile(hera_beam_filepath_X,'hera_frequency_array')
        hera_beam_in=Ptrarr(2)
        hera_beam_in[0]=getvar_savefile(hera_beam_filepath_X,'hera_beam_hpx',/pointer_return)
        hera_beam_in[1]=getvar_savefile(hera_beam_filepath_Y,'hera_beam_hpx',/pointer_return)
        
        ;hpx_inds=Lindgen(n_hpx)
        ;pix2vec_ring,nside,hpx_inds,pix_coords
        ;vec2ang,pix_coords,pix_za,pix_az ;returns RADIANS
               
        FOR pol_i=0,1 DO BEGIN
            hera_beam_interp=Fltarr(n_hpx,nfreq_bin)
            FOR hpx_i=0L,n_hpx-1 DO hera_beam_interp[hpx_i,*]=Interpol((*hera_beam_in[pol_i])[*,hpx_i],hera_frequency_array,freq_center)
            hera_beam_interp_arr=Ptrarr(nfreq_bin)
            FOR freq_i=0,nfreq_bin-1 DO hera_beam_interp_arr[freq_i]=Ptr_new(hera_beam_interp[*,freq_i])
            hera_beam_grid_arr=healpix_interpolate(hera_beam_interp_arr,obs,nside=nside,Jdate_use=Jdate_use,coord_sys='equatorial')
            FOR freq_i=0,nfreq_bin-1 DO Jones_matrix[pol_i,pol_i,freq_i]=Ptr_new((Interpolate(*hera_beam_grid_arr[freq_i],xvals_interp,yvals_interp)*horizon_mask)[pix_use])
            FOR freq_i=0,nfreq_bin-1 DO Jones_matrix[pol_i,(pol_i+1) mod 2,freq_i]=Ptr_new(Fltarr(n_pix))
        ENDFOR
    ENDELSE

ENDCASE
antenna.jones=Jones_matrix

RETURN,antenna
END
