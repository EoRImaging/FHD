FUNCTION visibility_psf,psf_base,psf_residuals_n,psf_residuals_i,psf_residuals_val,$
    freq_i=freq_i,baseline_i=baseline_i,xcenter=xcenter,ycenter=ycenter,$
    image_sample=image_sample,psf_i=psf_i,short_circuit_flag=short_circuit_flag,$
    xmin=xmin,ymin=ymin,xcen0=xcen0,ycen0=ycen0,polarization=polarization,dimension=dimension,elements=elements,$
    psf_dim=psf_dim,psf_resolution=psf_resolution
IF N_Elements(elements) EQ 0 THEN elements=dimension

IF N_Elements(xcenter) EQ 0 THEN BEGIN short_circuit_flag=1 & xcenter=0 & ycenter=0 & ENDIF
;If keyword image_sample is set, then this function will instead return Total(psf*image_sample) (this saves computations)

;high resolution nearest neighbor method

;DOES NOT YET USE RESIDUALS!!!
x_offset=Round((Ceil(xcenter)-xcenter)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycenter)-ycenter)*psf_resolution) mod psf_resolution
xcen0=Round(xcenter+x_offset/psf_resolution+dimension/2.) ;do this after offset, in case it has rounded to the next grid point
ycen0=Round(ycenter+y_offset/psf_resolution+elements/2.)
psf_use=*psf_base[polarization,freq_i,x_offset,y_offset]

xmin=Floor(xcen0-psf_dim/2.) & xmax=xmin+psf_dim-1
ymin=Floor(ycen0-psf_dim/2.) & ymax=ymin+psf_dim-1
IF Keyword_Set(short_circuit_flag) THEN RETURN, psf_use

CASE 1 OF
    Keyword_Set(image_sample) : result=Total(psf_use*image_sample[xmin:xmax,ymin:ymax])
    Keyword_Set(psf_i) : BEGIN
        psf_use_i=where(psf_use)
        result=psf_use[psf_use_i]
        psf_i=(meshgrid(psf_dim,psf_dim,1)+xmin)+(meshgrid(psf_dim,psf_dim,2)+ymin)*dimension
        psf_i=psf_i[psf_use_i]
    END
    ELSE : BEGIN
        result=fltarr(dimension,elements)
        result[xmin:xmax,ymin:ymax]=psf_use
    ENDELSE
ENDCASE

RETURN, result
END