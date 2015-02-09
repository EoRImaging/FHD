FUNCTION uvfits_params_simulate,hdr,params_in,sim_baseline_uu=sim_baseline_uu,sim_baseline_vv=sim_baseline_vv,$
    sim_baseline_ww=sim_baseline_ww,sim_baseline_i=sim_baseline_i,sim_baseline_time=sim_baseline_time,$
    sim_tile_locations_x=sim_tile_locations_x,sim_tile_locations_y=sim_tile_locations_y,sim_tile_i=sim_tile_i,_Extra=extra

;    params={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time}

n_tile=hdr.n_tile
n_baseline=hdr.nbaseline ;excludes time axis!

tile_sim=(N_Elements(sim_tile_locations_x)+N_Elements(sim_tile_locations_y)) GT 0
baseline_sim=(N_Elements(sim_baseline_uu)+N_Elements(sim_baseline_vv)+$
              N_Elements(sim_baseline_ww)+N_Elements(sim_baseline_time)) GT 0
IF tile_sim THEN baseline_sim=0
params_in_flag=Keyword_Set(params_in)

IF params_in_flag THEN BEGIN
    default_uu=params_in.uu
    default_vv=params_in.vv
    default_ww=params_in.ww
    default_i=params_in.baseline_arr
    default_time=params_in.time
ENDIF ELSE BEGIN

ENDELSE

IF baseline_sim THEN BEGIN
    IF N_Elements(sim_baseline_uu) EQ 0 THEN sim_baseline_uu=default_uu
    IF N_Elements(sim_baseline_vv) EQ 0 THEN sim_baseline_vv=default_vv
    IF N_Elements(sim_baseline_ww) EQ 0 THEN sim_baseline_ww=default_ww
    IF N_Elements(sim_baseline_i) EQ 0 THEN sim_baseline_i=default_i
    IF N_Elements(sim_baseline_time) EQ 0 THEN sim_baseline_time=default_time
    n_use=Minmax([N_Elements(sim_baseline_uu),N_Elements(sim_baseline_vv),N_Elements(sim_baseline_ww),$
               N_Elements(sim_baseline_i),N_Elements(sim_baseline_time)])
    IF n_use[0] NE n_use[1] THEN BEGIN
        n_use=n_use[0]
        sim_baseline_uu=sim_baseline_uu[0:n_use-1]
        sim_baseline_vv=sim_baseline_vv[0:n_use-1]
        sim_baseline_ww=sim_baseline_ww[0:n_use-1]
        sim_baseline_i=sim_baseline_i[0:n_use-1]
        sim_baseline_time=sim_baseline_time[0:n_use-1]
    ENDIF
ENDIF 



params={uu:sim_baseline_uu,vv:sim_baseline_vv,ww:sim_baseline_ww,$
        baseline_arr:sim_baseline_i,time:sim_baseline_time}
RETURN,params
END