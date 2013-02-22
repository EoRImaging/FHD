PRO source_array_export,source_array,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=file_path
IF N_Elements(radius) EQ 0 THEN radius=0.
IF N_Elements(Ires) EQ 0 THEN Ires=0.
IF N_Elements(Qres) EQ 0 THEN Qres=0.
IF N_Elements(beam_avg) EQ 0 THEN beam_avg_use=1. ELSE beam_avg_use=beam_avg[source_array.x,source_array.y]
header=['#id','x_loc','y_loc','RA','Dec','S/N','radius','avg_beam','XX_apparent','YY_apparent',$
    'Stokes_I_fit','Stokes_Q_fit','Stokes_I_res','Stokes_Q_res']
ns=(size(source_array,/dimension))[0]
source_arr_out=fltarr(14,ns)
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
Textfast,source_arr_out,header,/write,file_path=file_path

END