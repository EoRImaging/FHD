FUNCTION source_array_reformat,source_array,obs,base=base,parent=parent,beam=beam,stokes_images=stokes_images

radius=angle_difference(obs.obsdec,obs.obsra,source_array.dec,source_array.ra,/degree)
IF N_Elements(beam) EQ 0 THEN beam_avg_use=1. ELSE beam_avg_use=beam[source_array.x,source_array.y]

Ires=(Qres=fltarr(N_Elements(source_array)))
n_pol=obs.n_pol

IF N_Elements(stokes_images) GT 0 THEN BEGIN 
    cx=Round(source_array.x) & cy=Round(source_array.y)
    ind_use=where((cx<cy GE 0) AND (cx>cy LE (obs.dimension<obs.elements)-1))  
    Ires[ind_use]=(*stokes_images[0])[cx[ind_use],cy[ind_use]]
    IF n_pol GT 1 THEN Qres[ind_use]=(*stokes_images[1])[cx[ind_use],cy[ind_use]]
ENDIF

ns=(size(source_array,/dimension))[0]
source_arr_out=fltarr(21,ns)
source_arr_out[0,*]=source_array.id
source_arr_out[1,*]=source_array.x
source_arr_out[2,*]=source_array.y
source_arr_out[3,*]=source_array.ra
source_arr_out[4,*]=source_array.dec
source_arr_out[5,*]=source_array.ston
source_arr_out[6,*]=radius
source_arr_out[7,*]=beam_avg_use
source_arr_out[8,*]=source_array.freq
source_arr_out[9,*]=source_array.flux.xx
source_arr_out[10,*]=source_array.flux.yy
source_arr_out[11,*]=source_array.flux.xy
source_arr_out[12,*]=source_array.flux.yx
source_arr_out[13,*]=source_array.flux.I
source_arr_out[14,*]=source_array.flux.Q
source_arr_out[15,*]=source_array.flux.U
source_arr_out[16,*]=source_array.flux.V
source_arr_out[17,*]=Ires
source_arr_out[18,*]=Qres
source_arr_out[19,*]=source_array.flag
source_arr_out[20,*]=parent
IF Keyword_Set(base) THEN source_arr_out = [[base], [source_arr_out]]
RETURN,source_arr_out
END

PRO source_array_export,source_array,obs,beam=beam,stokes_images=stokes_images,file_path=file_path

IF N_Elements(file_path) EQ 0 THEN RETURN 
header=['#id',$ ; 0
        'x_loc',$ ; 1
        'y_loc',$ ; 2
        'RA',$ ; 3
        'Dec',$ ; 4
        'S/N',$ ; 5
        'radius',$ ; 6
        'avg_beam',$ ; 7
        'frequency',$ ; 8
        'XX_apparent',$ ; 9
        'YY_apparent',$ ; 10
        'XY_apparent',$ ; 11
        'YX_apparent',$ ; 12
        'Stokes_I',$ ; 13
        'Stokes_Q',$ ; 14
        'Stokes_U',$ ; 15
        'Stokes_V',$ ; 16
        'Stokes_I_res',$ ; 17
        'Stokes_Q_res',$ ; 18
        'Flag',$ ; 19
        'Parent'] ; 20
source_arr_out = source_array_reformat(source_array,obs,parent=-1,beam=beam,stokes_images=stokes_images)

extend_flag = where(Ptr_valid(source_array.extend),n_ext)
FOR i=0, n_ext-1 DO BEGIN
    ext_i = extend_flag[i]
    id = source_array[ext_i].id
    source_arr_out = source_array_reformat(*source_array[ext_i].extend,obs,parent=id,$
        base=source_arr_out,beam=beam,stokes_images=stokes_images)
ENDFOR
Textfast,source_arr_out,header,/write,file_path=file_path

END