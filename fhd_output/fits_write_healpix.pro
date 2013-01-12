PRO fits_write_healpix,data_arr,ra_vals=ra_vals,dec_vals=dec_vals,hpx_inds=hpx_inds,nside=nside,$
    frequency_list=frequency_list,filename=filename,directory=directory,stokes=stokes


output_filepath=filepath(filename,root=rootdir('mwa'),sub=directory)

n_pol=N_Elements(data_arr)
IF Keyword_Set(stokes) THEN pnames=(['I','Q','U','V'])[0:n_pol-1] ELSE pnames=(['XX','YY','XY','YX'])[0:n_pol-1] 

params=transpose(hpx_inds)
pscale=fltarr(1)+1.
Pzero=fltarr(1)
Ptype=Strarr(1)+'HPXIND'
IF Keyword_Set(ra_vals) THEN BEGIN params=[params,transpose(ra_vals)] & pscale=[pscale,1.] & pzero=[pzero,0.] & ptype=[ptype,'RA'] & ENDIF
IF Keyword_Set(dec_vals) THEN BEGIN params=[params,transpose(dec_vals)] & pscale=[pscale,1.] & pzero=[pzero,0.] & ptype=[ptype,'DEC'] & ENDIF

FOR pi=0,n_pol-1 DO BEGIN
    IF pi EQ 0 THEN create=1 ELSE create=0

    header=["COMMENT  Partial sky HEALPix images at multiple frequencies"]
    sxaddpar,header,'NSIDE',nside,'HEALPix nside parameter',before='END'
    IF Keyword_Set(stokes) THEN sxaddpar,header,'POL',pnames[pi],'Stokes parameter',before='END' $
        ELSE sxaddpar,header,'POL',pnames[pi],'Instrumental polarization',before='END'
    
    
    naxis=2
    ctype=Strarr(naxis)
    crval=Fltarr(naxis)
    crpix=Lon64arr(naxis)
    cdelt=Fltarr(naxis)
    ctype[0]='NONE'
    ctype[1]='Frequency'
    sxaddpar,header,'CTYPE1','NONE','Dimension created when using group parameters',before='END'
    
    sxaddpar,header,'CTYPE2','Frequency',before='END'
    sxaddpar,header,'CRVAL2',frequency_list[0]/1E6,'MHz',before='END'
    sxaddpar,header,'CRPIX2',0,before='END'
    sxaddpar,header,'CDELT2',(frequency_list[1]-frequency_list[0])/1E6,'MHz',before='END'
    
    MWRFITS, *data_arr[pi], output_filepath, Header,Group=params, Status=status,create=create, Pscale=Pscale, Pzero=Pzero, ptype=ptype,/silent
ENDFOR    

END