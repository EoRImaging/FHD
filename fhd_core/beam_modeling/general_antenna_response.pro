FUNCTION general_antenna_response,obs,antenna,za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim

n_ant=obs.n_tile
n_ant_pol=antenna[0].n_pol ;this needs to be the same for all antennas!
pix_use=*antenna[0].pix_use
icomp=DComplex(0,1)
c_light_vacuum=299792458.
DtoR=!DPi/180

response=Ptrarr(n_ant_pol,n_ant)
nfreq_bin=antenna[0].nfreq_bin
freq_center=antenna[0].freq

;Calculate projections only at locations of non-zero pixels
proj_east_use=Sin(za_arr[pix_use]*DtoR)*Sin(az_arr[pix_use]*DtoR)
proj_north_use=Sin(za_arr[pix_use]*DtoR)*Cos(az_arr[pix_use]*DtoR)
proj_z_use=Cos(za_arr[pix_use]*DtoR)

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
        
        response_grp=Ptrarr(nfreq_bin)
        FOR freq_i=0L,nfreq_bin-1 DO BEGIN
            response=DComplexarr(N_elements(pix_use))
            Kconv=(2.*!DPi)*(freq_center[freq_i]/c_light_vacuum) 
            voltage_delay=Exp(icomp*2.*!DPi*delays*(freq_center[freq_i])*Reform((*gain[pol_i])[freq_i,*])) 
            meas_current=(*coupling[pol_i,freq_i])#voltage_delay
            zenith_norm=Mean((*coupling[pol_i,freq_i])#Replicate(1.,n_ant_elements))
            meas_current/=zenith_norm

            FOR ii=0L,n_ant_elements-1 DO BEGIN
                antenna_gain_arr=Exp(-icomp*Kconv*D_d[*,ii])
                response+=(antenna_gain_arr*meas_current[ii]/n_ant_elements)
            ENDFOR
            response_grp[freq_i]=Ptr_new(response)
        ENDFOR
        FOR g_i=0L,ng-1 DO antenna[g_inds[g_i]].response[pol_i,*]=response_grp
    ENDFOR
ENDFOR

RETURN,antenna
END
