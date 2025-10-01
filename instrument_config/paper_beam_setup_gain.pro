FUNCTION paper_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,_extra=extra

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
n_tile=obs.n_tile
beam_model_version=Max(antenna.model_version)
xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)
pix_use=*antenna[0].pix_use
freq_center=antenna[0].freq ;all need to be identical, so just use the first
speed_light=299792458. ;speed of light, in meters/second
icomp=Complex(0,1)

paper_beam_filepath=filepath('paper_x_beam_nside128.fits',root=rootdir('FHD'),sub='instrument_config')
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

;build the instrumental pol Jones matrix
CASE beam_model_version OF
    2: BEGIN
        beam_cube=mrdfits(paper_beam_filepath,0,header,/silent)
        naxis1=sxpar(header,'NAXIS1')
        naxis2=sxpar(header,'NAXIS2')
        n_freq=sxpar(header,'NAXIS3')
        
        beam_RA_pix0=sxpar(header,'CRPIX1')-1
        beam_RA0=sxpar(header,'CRVAL1')
        beam_RA_delt=sxpar(header,'CDELT1')
        beam_Dec_pix0=sxpar(header,'CRPIX2')-1
        beam_Dec0=sxpar(header,'CRVAL2')
        beam_Dec_delt=sxpar(header,'CDELT2')
        
        freq_ref_i=sxpar(header,'CRPIX3')-1
        freq_ref_val=sxpar(header,'CRVAL3')
        freq_delt=sxpar(header,'CDELT3')
        freq_arr=(findgen(n_freq)-freq_ref_i)*freq_delt+freq_ref_val
        
        degpix=[beam_RA_delt,beam_Dec_delt]        
        
        
        ;beam_slice=Sqrt(beam_slice>0.)
        
        ;IF polarization EQ 1 THEN beam_slice=shift(Rotate(beam_slice,1),1,0)
        
        projection_slant_orthographic,astr=astr,degpix=degpix,obsra=beam_RA0,obsdec=beam_Dec0,zenra=beam_RA0,zendec=beam_Dec0,$
            dimension=naxis1,elements=naxis2,obsx=beam_RA_pix0+1,obsy=beam_Dec_pix0+1,zenx=beam_RA_pix0+1,zeny=beam_Dec_pix0+1
        xvals_use=xvals_instrument
        yvals_use=yvals_instrument
        mask_i=where((az_arr EQ 0) AND (za_arr EQ 90),n_mask)
        IF n_mask GT 0 THEN BEGIN
            xvals_use[mask_i]=!Values.F_NAN
            yvals_use[mask_i]=!Values.F_NAN
        ENDIF
        Jones_matrix=antenna.jones
        FOR pol_i=0,n_ant_pol-1 DO BEGIN
            IF pol_i EQ 1 THEN astr.cd=[[0.,1.],[1.,0.]] ELSE astr.cd=[[1.,0.],[0.,1.]]
            apply_astrometry, obs,ra_arr=xvals_use, dec_arr=yvals_use, x_arr=x_int, y_arr=y_int, JDate=obs.JD0, astr=astr, /ad2xy, /refraction ; This might be broken! If it is, it's probably a problem with JDate
;            ad2xy,xvals_use,yvals_use,astr,x_int,y_int
            FOR freq_i=0,nfreq_bin-1 DO BEGIN
                freq_i_test=Min(abs(freq_arr-freq_center[freq_i]),freq_i_use)
                beam_slice=Reform(beam_cube[*,*,freq_i_use])
                tile_beam=Float(interpolate(beam_slice,x_int,y_int,missing=0,cubic=-0.5))>0.
                Jones_matrix[pol_i,pol_i,freq_i]=Ptr_new((tile_beam)[pix_use])
                xpol_i=Abs(1-pol_i)
                Jones_matrix[xpol_i,pol_i,freq_i]=Ptr_new(Fltarr(N_elements(pix_use))) ;all zeroes
            ENDFOR
        ENDFOR
    END
    ELSE: BEGIN      
        print,"Using default beam model"
        antenna_height=antenna[0].height
        wavelength=speed_light/freq_center
        Jones_matrix=antenna.jones
        FOR freq_i=0,nfreq_bin-1 DO BEGIN
            groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength[freq_i])) ;should technically have zc_arr, but until that is nonzero this is the same and faster
            groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength[freq_i]) ;normalization factor
            groundplane/=groundplane0
            Jones_matrix[0,0,freq_i]=Ptr_new((Cos(za_arr*!DtoR)*Sin(az_arr*!DtoR)*groundplane)[pix_use])
            Jones_matrix[1,0,freq_i]=Ptr_new((Cos(az_arr*!DtoR)*groundplane)[pix_use])
            Jones_matrix[0,1,freq_i]=Ptr_new((Cos(za_arr*!DtoR)*Cos(az_arr*!DtoR)*groundplane)[pix_use])
            Jones_matrix[1,1,freq_i]=Ptr_new((-Sin(az_arr*!DtoR)*groundplane)[pix_use])
        ENDFOR
    ENDELSE
ENDCASE
antenna.jones=Jones_matrix


RETURN,antenna
END
