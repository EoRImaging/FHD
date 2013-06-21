PRO gamma_rm_test,n_avg=n_avg,phi=phi,_Extra=extra

IF N_Elements(n_avg) EQ 0 THEN n_avg=1.
IF N_Elements(phi) EQ 0 THEN phi=findgen(200)/2.-99.5

data_directory='D:\MWA\DATA2\Gamma\C102\141\
vis_file_path='D:\MWA\DATA2\Gamma\C102\141\1035663744_cal.uvfits'
fhd_file_path='D:\MWA\DATA2\Gamma\C102\141\fhd\1035663744'

obs_filepath=fhd_file_path+'_obs.sav'
psf_filepath=fhd_file_path+'_beams.sav'
restore,psf_filepath ;psf
restore,obs_filepath ;obs
;; *_fhd.sav contains:
;;residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
;;    beam_base,beam_correction,ra_arr,dec_arr,astr
;restore,fhd_file_path+'_fhd.sav'
dimension=obs.dimension
elements=obs.elements
ra_cen=float(ten(06,36,35.928))*15.
dec_cen=float(ten(-20,40,16.93))
radius=0.2
ad2xy,ra_cen,dec_cen,obs.astr,x_cen,y_cen

x_low=Floor(x_cen-radius/obs.degpix)
x_high=Ceil(x_cen+radius/obs.degpix)
y_low=Floor(y_cen-radius/obs.degpix)
y_high=Ceil(y_cen+radius/obs.degpix)
dimension_use=((x_high-x_low+1.)>(y_high-y_low+1.)
IF dimension_use mod 2 EQ 1 THEN dimension_use+=1
elements_use=dimension_use
x_range=Lindgen(dimension_use)+x_low 
y_range=Lindgen(elements_use)+y_low
inds=Rebin(x_range,dimension_use,elements_use,/sample)+Rebin(transpose(y_range)*dimension,dimension_use,elements_use)

dirty_arr=vis_model_freq_split(0,obs,psf,model_uv_arr=0,fhd_file_path=fhd_file_path,vis_file_path=vis_file_path,$
    n_avg=n_avg,timing=t_split,/fft,weights=weights_arr,variance=variance_arr,x_range=x_range,y_range=y_range,_Extra=extra) 


fi_use=where((*obs.baseline_info).freq_use)
freq_arr=((*obs.baseline_info).freq)[fi_use]
c_light=299792458.
n_freq=N_Elements(fi_use)
rbin=0
psf_dim=psf.dim
xl=dimension/2.-Floor(psf_dim/2.)+1
xh=dimension/2.-Floor(psf_dim/2.)+psf_dim
yl=elements/2.-Floor(psf_dim/2.)+1
yh=elements/2.-Floor(psf_dim/2.)+psf_dim

instr_cube=Fltarr(n_freq,dimension_use,elements_use,2)
weights_cube=Fltarr(n_freq,dimension_use,elements_use,2)
weights_threshold=0.05^2.
FOR fi=0L,n_freq-1 DO BEGIN
    FOR pol_i=0,1 DO BEGIN
        instr_cube[fi,*,*,pol_i]=(*dirty_arr[pol_i,fi_use[fi]])
        weights_cube[fi,*,*,pol_i]=extract_subarray(beam_image(psf,obs,pol_i=pol_i,freq_i=fi_use[fi],/square),x_range,y_range)
    ENDFOR
ENDFOR

weights_cube=weights_cube>weights_threshold ;make sure not to pass zeroes, but really should only look at regions where this isn't a problem
Stokes_Q=instr_cube[*,*,*,0]/weights_cube[*,*,*,0]-instr_cube[*,*,*,1]/weights_cube[*,*,*,1]
Stokes_U=Fltarr(n_freq,dimension_use,elements_use)
LAMBDASQ=(c_light/freq_arr)^2.

rmsynthesis, stokes_q,stokes_u,lambdasq,phi,fdf_cube
END