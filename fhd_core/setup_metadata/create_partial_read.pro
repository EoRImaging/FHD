; Build a selection structure for a partial UVH5 read.
;
; All index selections are zero-based and may be scalar or vector values.
; Antenna selection keeps a baseline when either ANTENNA1 or
; ANTENNA2 is one of the requested antenna numbers.
;
; Example:
;   partial_read = create_partial_read( $ 
;       freq_channels=[100L, 101L, 105L], $ 
;       time_channels=0L + INDGEN(20), $ 
;       polarisations=[0L, 3L], $ 
;       antenna=[1L, 2L])
;
FUNCTION create_partial_read, freq_channels=freq_channels, time_channels=time_channels, $ 
    polarisations=polarisations, antenna=antenna

  partial_read = {}
  if N_elements(freq_channels) GT 0 then begin
    if N_elements(partial_read) EQ 0 then partial_read={freq_channels:freq_channels} else $
      partial_read = structure_update(partial_read, _Extra={freq_channels:freq_channels})
  endif
  if N_elements(time_channels) GT 0 then begin
    if N_elements(partial_read) EQ 0 then partial_read={time_channels:time_channels} else $
      partial_read = structure_update(partial_read, _Extra={time_channels:time_channels})
  endif
  if N_elements(polarisations) GT 0 then begin
    if N_elements(partial_read) EQ 0 then partial_read={polarisations:polarisations} else $
      partial_read = structure_update(partial_read, _Extra={polarisations:polarisations})
  endif
  if N_elements(antenna) GT 0 then begin
    if N_elements(partial_read) EQ 0 then partial_read={antenna:antenna} else $
      partial_read = structure_update(partial_read, _Extra={antenna:antenna})
  endif

  RETURN, partial_read

END
