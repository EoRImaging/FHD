pro plot_cals,cal=cal,phase_filename=phase_filename,amp_filename=amp_filename
; Make plot of the cal solutions, save to png
; PS_START/PS_END write .ps first, then converts to png. Supply .png
; filename to automatically overwrite .ps.

tile_names = cal.tile_names

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

FOR tile=1,8 DO BEGIN
  FOR rec=1,16 DO BEGIN
    tile_name = 10*rec + tile ; correspond to cal structure's names
    tile_index = 8*(rec-1) + tile - 1 ; index starting at 0
    tilei = where(tile_names eq tile_name)
    IF (tilei eq -1) THEN BEGIN
      ; no tile found... must of been flagged
      cgplot,1,title=strtrim(tile_name,2),XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_index,*],/noerase,charsize=.5
    ENDIF ELSE BEGIN
      cgplot,freq,phunwrap(atan(gains0[*,tilei],/phase)),color='blue',title=strtrim(tile_name,2),$
          XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_index,*],yrange=[-1.5*!pi,1.5*!pi],$
             charsize=.5,/noerase
       cgoplot,freq,phunwrap(atan(gains1[*,tilei],/phase)),color='red'
    ENDELSE
  ENDFOR
ENDFOR

PS_END,/png
    
PS_START,amp_filename,scale_factor=2,/quiet,/nomatch

; TODO: More intelligent plot max - a single value can throw
;                                   everything off.
;max_amp = max(abs([gains0,gains1]))
max_amp = mean(abs([gains0,gains1])) + 2*stddev(abs([gains0,gains1]))
FOR tile=1,8 DO BEGIN
  FOR rec=1,16 DO BEGIN
    tile_name = 10*rec + tile ; correspond to cal structure's names
    tile_index = 8*(rec-1) + tile - 1 ; index starting at 0
    tilei = where(tile_names eq tile_name)
    IF (tilei eq -1) THEN BEGIN
      ; no tile found... must of been flagged
      cgplot,1,title=strtrim(tile_name,2),XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_index,*],/noerase,charsize=.5
    ENDIF ELSE BEGIN
      cgplot,freq,abs(gains0[*,tilei]),color='blue',title=strtrim(tile_name,2),$
          XTICKFORMAT="(A1)",YTICKFORMAT="(A1)",position=plot_pos[tile_index,*],yrange=[0,max_amp],/noerase,charsize=.5
      cgoplot,freq,abs(gains1[*,tilei]),color='red'
    ENDELSE
  ENDFOR
ENDFOR

PS_END,/png
END    
