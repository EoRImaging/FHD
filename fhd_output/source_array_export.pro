PRO source_array_export,source_array,obs,beam=beam,stokes_images=stokes_images,file_path=file_path

radius=angle_difference(obs.obsdec,obs.obsra,source_array.dec,source_array.ra,/degree)
Ires=(Qres=fltarr(N_Elements(source_array)))
n_pol=obs.n_pol

IF N_Elements(stokes_images) GT 0 THEN BEGIN 
    cx=Round(source_array.x) & cy=Round(source_array.y)
    ind_use=where((cx<cy GE 0) AND (cx>cy LE (obs.dimension<obs.elements)-1))  
    Ires[ind_use]=(*stokes_images[0])[cx[ind_use],cy[ind_use]]
    IF n_pol GT 1 THEN Qres[ind_use]=(*stokes_images[1])[cx[ind_use],cy[ind_use]]
ENDIF

IF N_Elements(beam) EQ 0 THEN beam_avg_use=1. ELSE beam_avg_use=beam[source_array.x,source_array.y]
header=['#id','x_loc','y_loc','RA','Dec','S/N','radius','avg_beam','XX_apparent','YY_apparent',$
    'Stokes_I_fit','Stokes_Q_fit','Stokes_I_res','Stokes_Q_res','Extended']
ns=(size(source_array,/dimension))[0]
source_arr_out=fltarr(15,ns)
source_arr_out[0,*]=source_array.id
source_arr_out[1,*]=source_array.x
source_arr_out[2,*]=source_array.y
source_arr_out[3,*]=source_array.ra
source_arr_out[4,*]=source_array.dec
source_arr_out[5,*]=source_array.ston
source_arr_out[6,*]=radius
source_arr_out[7,*]=beam_avg_use
source_arr_out[8,*]=source_array.flux.xx
source_arr_out[9,*]=source_array.flux.yy
source_arr_out[10,*]=source_array.flux.I
source_arr_out[11,*]=source_array.flux.Q
source_arr_out[12,*]=Ires
source_arr_out[13,*]=Qres
source_arr_out[14,*]=Ptr_valid(source_array.extend)
Textfast,source_arr_out,header,/write,file_path=file_path

END