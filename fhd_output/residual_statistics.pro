PRO residual_statistics,residual_image,obs,fhd,radius_inc=radius_inc,beam_base=beam_base,ston=ston,center=center,$
    file_path_base=file_path_base,no_ps=no_ps,_Extra=extra
;Need to specify both obs and fhd structures

compile_opt idl2,strictarrsubs  

IF cgHasImageMagick() EQ 0 THEN BEGIN
    print,"Imagemagick not found! Install from http://www.imagemagick.org/ to use this program."
    RETURN
ENDIF

dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
n_pol=obs.n_pol
IF N_Elements(fhd) EQ 0 THEN fhd=fhd_init(obs)
beam_threshold=fhd.beam_threshold
smooth_width=fhd.smooth_width

IF N_Elements(radius_inc) EQ 0 THEN radius_inc=10. ;degrees
IF N_Elements(ston) EQ 0 THEN ston=0. ;signal to noise source threshold that has been removed from residual_image

beam_mask=fltarr(dimension,elements)+1
beam_avg=fltarr(dimension,elements)+1
FOR pol_i=0,((n_pol-1)<1) DO BEGIN
    beam_i=region_grow(*beam_base[pol_i],dimension/2.+dimension*elements/2.,threshold=[beam_threshold,Max(*beam_base[pol_i])])
    beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1    
    beam_mask*=beam_mask0
    beam_avg*=*beam_base[pol_i]
ENDFOR
IF n_pol GT 1 THEN beam_avg=Sqrt(beam_avg>0)

res_Is=(residual_image*beam_avg-median(residual_image*beam_avg,smooth_width))

fail_i=where(res_Is EQ 0,n_fail)
IF n_fail GT 0 THEN beam_mask[fail_i]=0
beam_i=where(beam_mask)

r_vals=degpix*Sqrt((meshgrid(dimension,elements,1)-dimension/2.)^2.+(meshgrid(dimension,elements,2)-elements/2.)^2.)
i_use=where((r_vals LE radius_inc) AND (beam_mask GT 0),n_use)

beam_corr_avg=weight_invert(beam_avg)

noise_floor=Stddev(res_Is[beam_i]);*beam_corr_avg

residual_hist=histogram(res_Is[i_use],binsize=noise_floor/20.,locations=residual_vals)

;IF ston GT 0 THEN BEGIN
;    residual_vals_gauss=residual_vals
;    residual_gauss_fit=gaussfit(residual_vals_gauss,residual_hist,gauss_params,nterms=3)
;ENDIF ELSE BEGIN
    ;only fit negative side for the last cut 
    residual_vals_gauss0=residual_vals
    pos_i=where(residual_vals_gauss0 GT 0,npos)
    IF npos GT 3 THEN BEGIN ;must have more elements in npos than nterms in gaussfit
        iflip=(min(pos_i)-1-Abs(pos_i-min(pos_i)))>0
        residual_vals_gauss=residual_vals_gauss0
        residual_vals_gauss[pos_i]=-residual_vals_gauss0[iflip]
        residual_hist1=residual_hist
        residual_hist1[pos_i]=residual_hist[iflip]
        residual_gauss_fit=gaussfit(residual_vals_gauss,residual_hist1,gauss_params,nterms=3)
    ENDIF ELSE RETURN 
    
;ENDELSE
amp=gauss_params[0]
meanval=gauss_params[1]
sigma=gauss_params[2]

xlow=(meanval-10*sigma)>min(residual_vals)
xhigh=(meanval+10*sigma)<max(residual_vals)

IF Keyword_Set(center) THEN BEGIN
    xabs=Abs(xhigh)>Abs(xlow)
    xhigh=xabs
    xlow=-xabs
ENDIF

sig_str=String(format='("Gaussian sigma: ",F5.2)',sigma) ;& print,sig_str
center_str=String(format='("Gaussian mean: ",F5.2)',meanval); & print,center_str
amp_str=String(format='("Gaussian amplitude: ",F6.1)',amp) ;& print,amp_str
fwhm_str=String(format='("Gaussian FWHM: ",F5.2)',2*SQRT(2*ALOG(2))*gauss_params[2]) ;& print,fwhm_str
xtitle='Flux density (Jy)'
ytitle='Number of pixels'

title=String(format='(A," residual!C (S/N > ",A,")")',file_basename(file_path_base),Strtrim(Strn(ston)))
name=String(format='("_residual_histogram_StoN_",A)',Strtrim(Strn(Fix(ston)),2))

ymargin=[4,4]
psym_res=10
psym_gauss=0


cgPS_Open,filename=file_path_base+name+'.ps',/quiet,/nomatch,charsize=charsize,xsize=10.35,ysize=8
cgPlot,residual_vals,residual_hist,color='black',linestyle=0,/ylog,yrange=[0.1,max(residual_hist)],xrange=[xlow,xhigh],xtitle=xtitle,ytitle=ytitle,Title=title,ymargin=ymargin,psym=psym_res
cgPlot,residual_vals_gauss,residual_gauss_fit,/over,color='red',linestyle=2,psym=psym_gauss
Al_Legend,['Residual','Gaussian fit'],linestyle=[0,2],psym=[0,0],charsize=1.,color=['black','red'],/left
Al_Legend,[amp_str,center_str,sig_str],charsize=1.,color='black',/right

;IF !version.os_family EQ 'unix' THEN BEGIN
    IF Keyword_Set(no_ps) THEN cgPS_Close,Density=300,Resize=25.,/png,/DELETE_PS,/allow_transparent,/nomessage $
        ELSE cgPS_Close,Density=300,Resize=25.,/png,/allow_transparent,/nomessage
;ENDIF ELSE BEGIN
;    IF Keyword_Set(no_ps) THEN cgPS_Close,Density=300,Resize=25.,/png,/DELETE_PS,/allow_transparent,/nomessage $
;        ELSE cgPS_Close,Density=300,Resize=25.,/png,/NoWait,/allow_transparent,/nomessage
;ENDELSE

END