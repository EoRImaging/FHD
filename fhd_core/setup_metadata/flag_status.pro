PRO flag_status,obs
tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use,ncomplement=n_tile_cut)
freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use,ncomplement=n_freq_cut)
print,String(format='(A," frequency channels used and ",A," channels flagged")',$
    Strn(n_freq_use),Strn(n_freq_cut))
print,String(format='(A," tiles used and ",A," tiles flagged")',$
    Strn(n_tile_use),Strn(n_tile_cut))
IF Tag_exist(*obs.baseline_info,'time_use') THEN BEGIN
    time_use_i=where((*obs.baseline_info).time_use,n_time_use,ncomplement=n_time_cut)
    print,String(format='(A," time steps used and ",A," time steps flagged")',$
        Strn(n_time_use),Strn(n_time_cut))
ENDIF
END