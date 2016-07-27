PRO vis_reorder,hdr,params,vis_arr,vis_weights
; reorder visibilities to have ascending basline index, and add any baselines missing from some time steps

IF Min(Ptr_valid(vis_weights)) THEN flag_switch=1 ELSE flag_switch=0

n_pol=N_Elements(vis_arr)

n_freq=hdr.n_freq
n_tile=hdr.n_tile
bi_arr=params.baseline_arr
n_baselines_in=N_Elements(bi_arr)
time_arr=params.time
b0i=Uniq(time_arr)
time_vals=time_arr[b0i]
n_time=Float(N_Elements(b0i))
;have to put something in if there is only one time interval:
IF n_time GT 1 THEN time_step=(time_arr[b0i[1]]-time_arr[b0i[0]])*24.*3600. ELSE time_step=1. 
time_total=(Max(time_arr)-Min(time_arr))*24.*3600.
bin_start=fltarr(n_time) & IF n_time GT 1 THEN bin_start[1:*]=b0i[0:n_time-2]+1
bin_end=b0i
time_bin=fltarr(2,n_time) & time_bin[0,*]=bin_start & time_bin[1,*]=bin_end
bin_width=fltarr(n_time)
IF n_time GT 1 THEN bin_width[0]=b0i[0]+1 ELSE bin_width[0]=N_Elements(time_arr)
FOR i=1,n_time-1 DO bin_width[i]=b0i[i]-b0i[i-1]
bin_offset=Lonarr(n_time) & IF n_time GT 1 THEN bin_offset[1:*]=total(bin_width[0:n_time-2],/cumulative)    

bi_max=Max(bi_arr)
bi_hist=lonarr(bi_max,n_time)
;ri_arr=Ptrarr(n_time)

FOR t_i=0L,n_time-1 DO BEGIN
    bi_hist[*,t_i]=histogram(bi_arr[bin_start[t_i]:bin_end[t_i]],min=1,max=bi_max,/binsize)
;    ri_arr[t_i]=Ptr_new(Temporary(ri))
ENDFOR
bi_hist_tot=Total(bi_hist,2)

;name_mod=2.^((Ceil(Alog(Sqrt(Max(bin_width)*2.-n_tile))/Alog(2.)))>Floor(Alog(Min(bi_arr))/Alog(2.)))
;tile_A=Long(Floor(bi_arr/name_mod)) ;tile numbers start from 1
;tile_B=Long(Fix(bi_arr mod name_mod))
;n_tile_use=Max(tile_A)>Max(tile_B)
;auto_i=lindgen(n_tile_use)*name_mod+name_mod;+1 ; no '+1' since bi_hist starts from 1, not 0
;bi_hist_tot[auto_i]=bi_hist_tot[auto_i]>1 ;ensure that auto-correlations are included

bi_use=where(bi_hist_tot,n_baselines)
bi_map=lonarr(bi_max+1)
bi_map[bi_use+1]=lindgen(n_baselines)

n_baselines_int=n_baselines*n_time
bi_new=Rebin(bi_use+1,n_baselines,n_time)
bi_new=Reform(bi_new,n_baselines_int)
bi_order=lonarr(n_baselines_in)

FOR t_i=0L,n_time-1 DO bi_order[bin_start[t_i]:bin_end[t_i]]=$
    bi_map[bi_arr[bin_start[t_i]:bin_end[t_i]]]+n_baselines*t_i

FOR pol_i=0,n_pol-1 DO BEGIN
    vis_use=Complexarr(n_freq,n_baselines_int)
    vis_use[*,bi_order]=Temporary(*vis_arr[pol_i])
    vis_arr[pol_i]=Ptr_new(Temporary(vis_use))
    IF flag_switch THEN BEGIN
        vis_weight_use=fltarr(n_freq,n_baselines_int)
        vis_weight_use[*,bi_order]=Temporary(*vis_weights[pol_i])
        vis_weights[pol_i]=Ptr_new(Temporary(vis_weight_use))
    ENDIF
ENDFOR

time_new=time_vals[Floor(lindgen(n_baselines_int)/n_baselines)]
uu_new=(vv_new=(ww_new=fltarr(n_baselines_int)))
uu_new[bi_order]=params.uu
vv_new[bi_order]=params.vv
ww_new[bi_order]=params.ww
params={uu:uu_new,vv:vv_new,ww:ww_new,baseline_arr:bi_new,time:time_new}

hdr.nbaselines=n_baselines_int
END