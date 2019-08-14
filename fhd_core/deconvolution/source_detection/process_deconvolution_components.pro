FUNCTION process_deconvolution_components,component_array,obs,fhd_params,noise_map,beam_arr=beam_arr,$
    independent_fit=independent_fit,gain_factor=gain_factor,source_mask=source_mask,_Extra=extra

dimension = obs.dimension
elements = obs.elements
n_pol = obs.n_pol

beam_width=beam_width_calculate(obs,min_restored_beam_width=1.,/FWHM)
; Note that beam_arr contains the power beam, i.e. the beam squared
beam_avg = *beam_arr[0]
IF n_pol GT 1 THEN BEGIN
    beam_avg += *beam_arr[1]
    beam_avg /= 2.
ENDIF
beam_avg = sqrt(beam_avg)

noise_map=fhd_params.convergence*weight_invert(beam_avg, fhd_params.beam_threshold)
IF Keyword_Set(independent_fit) THEN noise_map*=Sqrt(n_pol)

comp_i_use=where(component_array.flux.I GT 0)
component_array=component_array[comp_i_use]
source_mask=Rebin(source_mask,dimension,elements,/sample)
source_array=components2sources(component_array,obs,fhd_params,radius=beam_width>1.5,noise_map=noise_map,$
    gain_factor=gain_factor,source_mask=source_mask,_Extra=extra)

; Add beam information to the source and component lists
FOR pol_i=0,n_pol-1 DO BEGIN
    beam_use = sqrt(*beam_arr[pol_i])
    component_array.beam.(pol_i) = beam_use[Round(component_array.x), Round(component_array.y)]
    source_array.beam.(pol_i) = beam_use[Round(source_array.x), Round(source_array.y)]
    ;also convert extended source components.
    extend_i=where(Ptr_valid(source_array.extend),n_ext)
    IF n_ext GT 0 THEN FOR ext_i=0L,n_ext-1 DO (*source_array[extend_i[ext_i]].extend).beam.(pol_i) = source_array[extend_i[ext_i]].beam.(pol_i)
ENDFOR

RETURN, source_array
END
