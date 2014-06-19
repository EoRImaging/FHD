PRO shapelet_model_uv,input
;takes shaplet models of extended sources and generates a uv model for use by FHD
;method: generate Stokes I image of shaplet from coefficients, convert to instrumental pol, FFT to uv space

shapelet_path='C:\mwa\IDL_code\FHD\catalog_data\shapelets\'
;textfast,parameters,/read,file_path=shapelet_path+'parameters'
position_angle=77.9922-90. 
major_axis=0.0023351807*!Radeg ;degrees
minor_axis=0.0014022673*!Radeg ;degrees
freq=180E6
ra = 3.360978 
dec = -37.150969
unit_conv=1.;!DtoR

textfast,coefficients,/read,file_path=shapelet_path+'FornaxA_180M_n25_nosort'
n1=coefficients[0,*]
n2=coefficients[1,*]
n_cf=N_Elements(n1)
f_n1n2=Double(coefficients[2,*])
N_max=Max(n1)>Max(n2)
obs_path='C:\mwa\DATA3\128T\testcal2\fhd_v45\1061327032_obs.sav'
restore,obs_path
degpix=obs.degpix
shapelet_dim=2.*Ceil(major_axis*unit_conv/(0.9*degpix*unit_conv)*n_max^(0.52))


IF shapelet_dim GT 1024 THEN BEGIN
    print,'ERROR: dimension too large!'
    RETURN
ENDIF

xvals=(meshgrid(shapelet_dim,shapelet_dim,1)-shapelet_dim/2)*degpix*unit_conv
yvals=(meshgrid(shapelet_dim,shapelet_dim,2)-shapelet_dim/2)*degpix*unit_conv

xvals=rot(xvals,position_angle,1.,shapelet_dim/2,shapelet_dim/2,/pivot,/interp)
yvals=rot(yvals,position_angle,1.,shapelet_dim/2,shapelet_dim/2,/pivot,/interp)
shapelet_path='C:\mwa\IDL_code\FHD\catalog_data\shapelets\'
textfast,hermite_coeffs,/read,file_path=shapelet_path+'hermite_coeffs'
hermite_coeffs=Double(hermite_coeffs)

model=Fltarr(shapelet_dim,shapelet_dim)
FOR i=0,n_cf-1 DO model+=f_n1n2[i]*shapelet_basis_fn(xvals,yvals,major_axis*unit_conv,minor_axis*unit_conv,hermite_coeffs,n1[i],n2[i])
model/=degpix^2. ;convert model units from Jy/beam to Jy
;local_x=meshgrid(

;RETURN,model_arr
END