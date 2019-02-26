FUNCTION general_antenna_response,obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim

n_ant=obs.n_tile
n_ant_pol=antenna[0].n_pol ;this needs to be the same for all antennas!
icomp=Complex(0,1)
c_light_vacuum=299792458.

response=Ptrarr(n_ant_pol,n_ant)
nfreq_bin=antenna[0].nfreq_bin
freq_center=antenna[0].freq

proj_east=Sin(za_arr*!DtoR)*Sin(az_arr*!DtoR) & proj_east_use=Reform(proj_east,(psf_image_dim)^2.)
proj_north=Sin(za_arr*!DtoR)*Cos(az_arr*!DtoR) & proj_north_use=Reform(proj_north,(psf_image_dim)^2.)
proj_z=Cos(za_arr*!DtoR) & proj_z_use=Reform(proj_z,(psf_image_dim)^2.)

;;initialize antenna structure
;antenna_str={n_pol:n_ant_pol,antenna_type:instrument,model_version:beam_model_version,freq:freq_center,nfreq_bin:nfreq_bin,$
;    n_ant_elements:0,Jones:Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin),coupling:Ptrarr(n_ant_pol,nfreq_bin),gain:Ptrarr(n_ant_pol),coords:Ptrarr(3),$
;    delays:Ptr_new(),size_meters:0.,height:0.,group_id:Lonarr(n_ant_pol)-1}

group_arr=antenna.group_id
FOR pol_i=0,n_ant_pol-1 DO BEGIN
    g_hist=histogram(group_arr[pol_i,*],min=0,/binsize,reverse_ind=g_ri)
    n_group=N_Elements(g_hist)
    FOR grp_i=0L,n_group-1 DO BEGIN
        ng=g_hist[grp_i]
        IF ng EQ 0 THEN CONTINUE
        g_inds=g_ri[g_ri[grp_i]:g_ri[grp_i+1]-1]
        ref_i=g_inds[0]
        n_ant_elements=antenna[ref_i].n_ant_elements
        coupling=antenna[ref_i].coupling
        gain=antenna[ref_i].gain
        xc_arr=*(antenna[ref_i].coords[0])
        yc_arr=*(antenna[ref_i].coords[1])
        zc_arr=*(antenna[ref_i].coords[2])
        delays=*(antenna[ref_i].delays)
        
        ;phase of each dipole for the source (relative to the beamformer settings)
        D_d=(proj_east_use#xc_arr+proj_north_use#yc_arr+proj_z_use#zc_arr)
        D_d=Reform(D_d,psf_image_dim,psf_image_dim,n_ant_elements)
        
        response_grp=Ptrarr(nfreq_bin)
        FOR freq_i=0L,nfreq_bin-1 DO BEGIN
            gain_sum = Total((*gain[pol_i])[freq_i,*])
            response=Complexarr(psf_image_dim,psf_image_dim)
            Kconv=(2.*!Pi)*(freq_center[freq_i]/c_light_vacuum) 
            antenna_gain_arr=Exp(-icomp*Kconv*D_d)
            voltage_delay=Exp(icomp*2.*!Pi*delays*(freq_center[freq_i])) 
            meas_current=(*coupling[pol_i,freq_i])#voltage_delay
            zenith_norm=Mean((*coupling[pol_i,freq_i])#Replicate(1.,n_ant_elements))
            meas_current/=zenith_norm

            FOR ii=0L,n_ant_elements-1 DO BEGIN
                response += ((*gain[pol_i])[freq_i,ii])*antenna_gain_arr[*,*,ii]*meas_current[ii]/gain_sum
            ENDFOR
            response_grp[freq_i]=Ptr_new(response)
        ENDFOR
        FOR g_i=0L,ng-1 DO antenna[g_inds[g_i]].response[pol_i,*]=response_grp
    ENDFOR
ENDFOR
RETURN,antenna
END
