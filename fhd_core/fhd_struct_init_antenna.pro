FUNCTION fhd_struct_init_antenna


antenna_spacing=1.1 ;meters (Same as M&C SetDelays script) ; Was 1.071 before? Verified in Tingay et al 2013
antenna_length=29.125*2.54/100. ;meters (measured) (NOT USED)
antenna_height=0.29 ;meters (June 2014 e-mail from Brian Crosse) ; Was 0.35 before
velocity_factor=0.673

antenna={model_version:beam_model_version,spacing:antenna_spacing,length:antenna_length,height:antenna_height,velocity_factor:velocity_factor,freq:freq_arr,$
    Jones:Jones_matrix,coupling:mutual_coupling,gain:gain_arr,phased_array_flag:phased_array_flag}
RETURN,antenna
END