PRO array_simulator_init,obs,params,error=error,sim_from_uvfits_filepath=sim_from_uvfits_filepath,$
    sim_from_fhd_filepath=sim_from_fhd_filepath,simulate_header=simulate_header,simulate_baselines=simulate_baselines,$
    instrument=instrument,_Extra=extra

CASE 1 OF
    Keyword_Set(sim_from_fhd_filepath): BEGIN
        file_path_fhd=sim_from_fhd_filepath
        fhd_save_io,status_str,file_path_fhd=file_path_fhd,var='status_str',_Extra=extra
        IF status_str.obs EQ 0 OR status_str.params EQ 0 THEN BEGIN
            error=1
            print,'Specified path is not a valid directory of FHD output: '+sim_from_fhd_filepath
        ENDIF ELSE BEGIN
            fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
            fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
        ENDELSE
        RETURN
    END
    Keyword_Set(sim_from_uvfits_filepath): BEGIN
        file_path_vis=sim_from_uvfits_filepath
        uvfits_read,hdr,params,file_path_vis=file_path_vis,n_pol=0,silent=silent,_Extra=extra
    END
    ELSE: ;do nothing
ENDCASE

IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
IF Keyword_Set(simulate_header) OR ~Keyword_Set(hdr) THEN hdr=uvfits_header_simulate(hdr,_Extra=extra)

IF Keyword_Set(simulate_baselines) OR ~Keyword_Set(params) THEN params=uvfits_params_simulate(hdr,params,_Extra=extra)

IF N_Elements(file_path_vis) EQ 0 THEN file_path_vis='simulation'
obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,instrument=instrument,_Extra=extra)
END