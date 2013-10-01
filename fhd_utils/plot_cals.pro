pro plot_cals,cal,obs,phase_filename=phase_filename,amp_filename=amp_filename,vis_baseline_hist=vis_baseline_hist,vis_hist_filename=vis_hist_filename
; Make plot of the cal solutions, save to png
; PS_START/PS_END write .ps first, then converts to png. Supply .png
; filename to automatically overwrite .ps.

tile_names = cal.tile_names
n_tiles=obs.n_tile
obs2=*obs.baseline_info
tile_use=obs2.tile_use

gains0 = *cal.gain[0] ; save on typing
gains1 = *cal.gain[1]

; find the non-flagged frequencies
freqi_use = where(total(abs(gains0),2) ne cal.n_tile) ; if total is exactly the number of tiles, it's probably set to exactly 1

gains0=gains0[freqi_use,*]
gains1=gains1[freqi_use,*]
freq=cal.freq[freqi_use]

plot_pos=calculate_plot_positions(.8, nplots=128, /no_colorbar, ncol=16, nrow=8, plot_margin = [.05, .05, .02, .25])
plot_pos=plot_pos.plot_pos

PS_START,phase_filename,scale_factor=2,/quiet,/nomatch

n_baselines=obs2.bin_offset[1]
tile_A=obs2.tile_A[0:n_baselines-1]
tile_B=obs2.tile_B[0:n_baselines-1]
tile_exist=(histogram(tile_A,min=0,/bin,max=(max(tile_A)>max(tile_B)))+histogram(tile_B,min=0,/bin,max=(max(tile_A)>max(tile_B))))<1

FOR tile_i=0L,n_tiles-1 DO BEGIN
    tile_name=tile_names[tile_i]
    rec=Floor(tile_name/10)
    tile=tile_name mod 10
    
    IF tile_exist[tile_i] EQ 0 THEN BEGIN
      ; no tile found... must have been flagged in pre-processing
      axiscolor='yellow'
      cgplot,1,title=strtrim(tile_name,2),XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_i,*],$
        /noerase,charsize=.5,axiscolor=axiscolor
    ENDIF ELSE BEGIN
      IF tile_use[tile_i] EQ 0 THEN axiscolor='red' ELSE axiscolor='black'
      cgplot,freq,phunwrap(atan(gains0[*,tile_i],/phase)),color='blue',title=strtrim(tile_name,2),$
          XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_i,*],yrange=[-1.5*!pi,1.5*!pi],$
          charsize=.5,/noerase,axiscolor=axiscolor
       cgoplot,freq,phunwrap(atan(gains1[*,tile_i],/phase)),color='red'
    ENDELSE
ENDFOR

PS_END,/png,Density=75,Resize=100.,/allow_transparent,/nomessage
    
PS_START,amp_filename,scale_factor=2,/quiet,/nomatch

; TODO: More intelligent plot max - a single value can throw
;                                   everything off.
;max_amp = max(abs([gains0,gains1]))
max_amp = mean(abs([gains0,gains1])) + 2*stddev(abs([gains0,gains1]))
FOR tile_i=0L,n_tiles-1 DO BEGIN
    tile_name=tile_names[tile_i]
    rec=Floor(tile_name/10)
    tile=tile_name mod 10
    
    IF tile_exist[tile_i] EQ 0  THEN BEGIN
      ; no tile found... must have been flagged in pre-processing
      axiscolor='yellow'
      cgplot,1,title=strtrim(tile_name,2),XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_i,*],$
        /noerase,charsize=.5,axiscolor=axiscolor
    ENDIF ELSE BEGIN
      IF tile_use[tile_i] EQ 0 THEN axiscolor='red' ELSE axiscolor='black'
      cgplot,freq,abs(gains0[*,tile_i]),color='blue',title=strtrim(tile_name,2),$
          XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_i,*],yrange=[0,max_amp],$
          /noerase,charsize=.5,axiscolor=axiscolor
      cgoplot,freq,abs(gains1[*,tile_i]),color='red'
    ENDELSE
ENDFOR

PS_END,/png,Density=75,Resize=100.,/allow_transparent,/nomessage

IF Keyword_Set(vis_baseline_hist) and Keyword_Set(vis_hist_filename) THEN BEGIN
   ratio=vis_baseline_hist.vis_res_ratio_mean ; just save some typing
   sigma=vis_baseline_hist.vis_res_sigma
   base_len=vis_baseline_hist.baseline_length

   PS_START,filename=vis_hist_filename,/quiet,/nomatch
   !p.multi=[0,2,1]
   FOR pol=0,1 DO BEGIN
      ;cgplot,base_len,ratio[pol,*],/xlog,yrange=[0,max(ratio+sigma)]
      ;cgerrplot,base_len,ratio[pol,*]-sigma[pol,*],ratio[pol,*]+sigma[pol,*]
      ;cgoplot,base_len,ratio[pol,*],color='red'
      cgplot,base_len,ratio[pol,*],color='red',/xlog,yrange=[0,max(ratio+sigma)]
      cgoplot,base_len,ratio[pol,*]+sigma[pol,*],linestyle=2
      cgoplot,base_len,ratio[pol,*]-sigma[pol,*],linestyle=2
   ENDFOR
   PS_END,/png,Density=75,Resize=100.,/allow_transparent,/nomessage
ENDIF

END    
