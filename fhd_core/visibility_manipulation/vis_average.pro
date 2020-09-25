PRO vis_average,vis_arr,vis_weights,params,hdr,vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,timing=timing
;need to modify params if averaging in time (no freq dependence) 
;need to modify hdr if averaging in frequency (no time dependence)
t0=Systime(1)
n_pol=hdr.n_pol<N_Elements(vis_arr)

time=params.time
b0i=Uniq(time)
n_baseline_time=N_Elements(time)
n_time0=N_Elements(b0i)
;bin_start=fltarr(n_time0) & bin_start[1:*]=b0i[0:n_time0-2]+1
;bin_end=b0i
;time_bin=fltarr(2,n_time0) & time_bin[0,*]=bin_start & time_bin[1,*]=bin_end
bin_width=fltarr(n_time0)
bin_width[0]=b0i[0]+1
FOR i=1,n_time0-1 DO bin_width[i]=b0i[i]-b0i[i-1]
bin_offset=fltarr(n_time0) & bin_offset[1:*]=total(bin_width[0:n_time0-2],/cumulative)    
n_baselines=bin_width[0]

;hdr={n_params:n_grp_params,nbaselines:nbaselines,n_tile:n_tile,n_pol:n_polarizations,n_freq:n_frequencies,$
;    freq_res:freq_res,freq_arr:frequency_array,obsra:obsra,obsdec:obsdec,date:date_obs,$
;    uu_i:uu_i,vv_i:vv_i,ww_i:ww_i,baseline_i:baseline_i,date_i:date_i,jd0:Jdate0,$
;    pol_dim:pol_dim,freq_dim:freq_dim,real_index:real_index,imaginary_index:imaginary_index,weights_index:weights_index}
IF Keyword_Set(vis_freq_average) THEN BEGIN
    n_freq=Floor(hdr.n_freq/vis_freq_average)
    hdr.n_freq=n_freq
    hdr.freq_res=hdr.freq_res*vis_freq_average
    freq_arr=hdr.freq_arr
    freq_arr=Fltarr(n_freq)
    FOR fi=0L,n_freq-1 DO freq_arr[fi]=Mean(hdr.freq_arr[fi*vis_freq_average:(fi+1)*vis_freq_average-1])
    hdr=structure_update(hdr,freq_arr=freq_arr)
    
;    hdr.freq_ref_i=Floor(hdr.freq_ref_i/vis_freq_average)
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        vis_old=Temporary(*vis_arr[pol_i])
        vis_weight_old=Temporary(*vis_weights[pol_i])>0
        *vis_arr[pol_i]=Complexarr(n_freq,n_baseline_time)
        FOR fi=0L,n_freq-1 DO BEGIN
            (*vis_arr[pol_i])[fi,*]=Total(vis_old[fi*vis_freq_average:(fi+1)*vis_freq_average-1,*]$
                *vis_weight_old[fi*vis_freq_average:(fi+1)*vis_freq_average-1,*],1)$
                *weight_invert(Total(vis_weight_old[fi*vis_freq_average:(fi+1)*vis_freq_average-1,*],1))
        ENDFOR
        vis_old=0 ;free memory
        *vis_weights[pol_i]=Fltarr(n_freq,n_baseline_time)
        FOR fi=0L,n_freq-1 DO BEGIN
            (*vis_weights[pol_i])[fi,*]=Total(vis_weight_old[fi*vis_freq_average:(fi+1)*vis_freq_average-1,*],1)
        ENDFOR
        vis_weight_old=0
    ENDFOR
ENDIF
;params={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time}
IF Keyword_Set(vis_time_average) THEN BEGIN
    n_freq=hdr.n_freq
    vis_time_average=Float(vis_time_average) ;make sure floating point division is used later
    uu0=Reform(params.uu,n_baselines,n_time0)
    vv0=Reform(params.vv,n_baselines,n_time0)
    ww0=Reform(params.ww,n_baselines,n_time0)
    baseline_arr0=Reform(params.baseline_arr,n_baselines,n_time0)
    time_arr0=Reform(params.time,n_baselines,n_time0)
    antenna1_arr0=Reform(params.antenna1,n_baselines,n_time0)
    antenna2_arr0=Reform(params.antenna2,n_baselines,n_time0)
    params=0
    n_time=Floor(n_time0/vis_time_average)
    uu_arr=Make_array(n_baselines*n_time,type=Size(uu0,/type))
    vv_arr=Make_array(n_baselines*n_time,type=Size(vv0,/type))
    ww_arr=Make_array(n_baselines*n_time,type=Size(ww0,/type))
    baseline_arr=Make_array(n_baselines*n_time,type=Size(baseline_arr0,/type))
    time_arr=Make_array(n_baselines*n_time,type=Size(time_arr0,/type))
    antenna1_arr=Make_array(n_baselines*n_time,type=Size(antenna1_arr0,/type))
    antenna2_arr=Make_array(n_baselines*n_time,type=Size(antenna2_arr0,/type))
    FOR ti=0L,n_time-1 DO BEGIN
        ;uu_arr[bin_offset[ti]] uses the most efficient IDL subscript notation to fill uu_arr starting at element bin_offset[ti]
        uu_arr[bin_offset[ti]]=Total(uu0[*,ti*vis_time_average:(ti+1)*vis_time_average-1],2)/vis_time_average
        vv_arr[bin_offset[ti]]=Total(vv0[*,ti*vis_time_average:(ti+1)*vis_time_average-1],2)/vis_time_average
        ww_arr[bin_offset[ti]]=Total(ww0[*,ti*vis_time_average:(ti+1)*vis_time_average-1],2)/vis_time_average
        baseline_arr[bin_offset[ti]]=Total(baseline_arr0[*,ti*vis_time_average:(ti+1)*vis_time_average-1],2)/vis_time_average
        time_arr[bin_offset[ti]]=Total(time_arr0[*,ti*vis_time_average:(ti+1)*vis_time_average-1],2)/vis_time_average
        antenna1_arr[bin_offset[ti]]=antenna1_arr0[*,ti*vis_time_average]
        antenna2_arr[bin_offset[ti]]=antenna2_arr0[*,ti*vis_time_average]
    ENDFOR
    params={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time_arr,antenna1:antenna1_arr,antenna2:antenna2_arr}
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        vis_old=Reform(Temporary(*vis_arr[pol_i]),n_freq,n_baselines,n_time0)
        weights_old=Reform(Temporary(*vis_weights[pol_i]),n_freq,n_baselines,n_time0)>0
        vis_new=Make_array(n_freq,n_baselines*n_time,type=Size(vis_old,/type))
        FOR ti=0L,n_time-1 DO vis_new[*,ti*n_baselines:(ti+1)*n_baselines-1]=$
            Total(vis_old[*,*,ti*vis_time_average:(ti+1)*vis_time_average-1]*weights_old[*,*,ti*vis_time_average:(ti+1)*vis_time_average-1],3)$
            /Total(weights_old[*,*,ti*vis_time_average:(ti+1)*vis_time_average-1],3)
        vis_old=0
        *vis_arr[pol_i]=Temporary(vis_new)
        
        weights_new=Make_array(n_freq,n_baselines*n_time,type=Size(weights_old,/type))
        FOR ti=0L,n_time-1 DO weights_new[*,ti*n_baselines:(ti+1)*n_baselines-1]=Total(weights_old[*,*,ti*vis_time_average:(ti+1)*vis_time_average-1],3)/vis_time_average
        weights_old=0
        *vis_weights[pol_i]=Temporary(weights_new)
    ENDFOR
ENDIF

timing=Systime(1)-t0
END
