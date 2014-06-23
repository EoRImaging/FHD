FUNCTION fhd_struct_init_antenna,obs,beam_model_version=beam_model_version,antenna_spacing=antenna_spacing,$
    antenna_length=antenna_length,antenna_height=antenna_height,velocity_factor=velocity_factor,freq_arr=freq_arr,$
    Jones_matrix=Jones_matrix,mutual_coupling=mutual_coupling,gain_arr=gain_arr,phased_array_flag=phased_array_flag,antenna_coords=antenna_coords


antenna={model_version:beam_model_version,spacing:antenna_spacing,length:antenna_length,height:antenna_height,velocity_factor:velocity_factor,freq:freq_arr,$
    Jones:Jones_matrix,coupling:mutual_coupling,gain:gain_arr,phased_array_flag:phased_array_flag,coords:antenna_coords}

RETURN,antenna
END