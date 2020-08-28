FUNCTION split_vis_weights,obs,vis_weights,bi_use=bi_use,preserve_weights=preserve_weights,even_only=even_only,odd_only=odd_only,$
    debug_evenoddsplit_integration=debug_evenoddsplit_integration,_Extra=extra
;function to create split even and odd time samples with identical flagging

n_pol=N_Elements(vis_weights)
IF Keyword_Set(preserve_weights) THEN vis_weights_use=pointer_copy(vis_weights) ELSE vis_weights_use=vis_weights
bin_start=(*obs.baseline_info).bin_offset
nt=obs.n_time
IF nt LT 2 THEN RETURN,vis_weights_use
nb=(size(*vis_weights_use[0],/dimension))[1]
bin_end=fltarr(nt)
bin_end[0:nt-2]=bin_start[1:nt-1]-1
bin_end[nt-1]=nb-1
bin_i=lonarr(nb)-1
nt2=Floor(nt/2)
FOR t_i=0,2*nt2-1 DO bin_i[bin_start[t_i]:bin_end[t_i]]=t_i

time_use=(*obs.baseline_info).time_use
time_start_i=Min(where(time_use))
nt3=Floor((nt-time_start_i)/2)*2
time_use_0=time_use[time_start_i:time_start_i+nt3-1:2]
time_use_1=time_use[time_start_i+1:time_start_i+nt3-1:2]
time_use_01=time_use_0*time_use_1
time_use*=0
time_use[time_start_i:time_start_i+nt3-1:2]=time_use_01
time_use[time_start_i+1:time_start_i+nt3-1:2]=time_use_01
time_cut_i=where(time_use LE 0,nt_cut)
IF nt_cut GT 0 THEN BEGIN
    FOR cut_i=0,nt_cut-1 DO BEGIN
        bin_i_cut=where(bin_i EQ time_cut_i[cut_i],n_cut)
        ; will be skipped by using where(bin_i mod 2 EQ 0,1) below (-1 mod 2 is still -1)
        IF n_cut GT 0 THEN bin_i[bin_i_cut]=-1
    ENDFOR
ENDIF

IF Keyword_Set(debug_evenoddsplit_integration) THEN BEGIN
    divisor=debug_evenoddsplit_integration/obs.time_res
    print,"DEBUG option: even/odd split integrated to "+Strn(debug_evenoddsplit_integration)+"s ("+Strn(divisor)+" time steps)"
    bin_i=Floor(bin_i/divisor)
ENDIF

bi_use=Ptrarr(2,/allocate)
*bi_use[0]=where(bin_i mod 2 EQ 0,n_even)
*bi_use[1]=where(bin_i mod 2 EQ 1,n_odd)

IF n_even LT n_odd THEN *bi_use[1]=(*bi_use[1])[0:n_even-1]
IF n_odd LT n_even THEN *bi_use[0]=(*bi_use[0])[0:n_odd-1]

FOR pol_i=0,n_pol-1 DO BEGIN
    ;Logic AND where the max is 1 and min is 0. Breaks down if there are negative weights (accounted for earlier)
    flag_use0=0>(*vis_weights_use[pol_i])[*,*bi_use[0]]<(*vis_weights_use[pol_i])[*,*bi_use[1]]<1
    *vis_weights_use[pol_i]*=0
    IF ~Keyword_Set(odd_only) THEN (*vis_weights_use[pol_i])[*,*bi_use[0]]=flag_use0
    IF ~Keyword_Set(even_only) THEN (*vis_weights_use[pol_i])[*,*bi_use[1]]=flag_use0 
    flag_use0=0 ;free memory
ENDFOR
RETURN,vis_weights_use
END
