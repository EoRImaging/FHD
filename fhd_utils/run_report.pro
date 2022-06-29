PRO run_report, start_mem, t0, silent=silent

  end_mem = (MEMORY(/HIGHWATER) - start_mem)/1e9
  timing=Systime(1)-t0
  IF ~Keyword_Set(silent) THEN BEGIN
    print,'Memory required (GB): ',Strn(Round(end_mem))
    print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
  ENDIF

END
