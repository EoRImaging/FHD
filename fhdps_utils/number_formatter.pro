;; little function to remove excess 0's and spaces from number strings
;;
;; format keyword will be passed to original string call
;; flag is passed to strtrim to define how spaces are removed, default is to remove both leading & trailing spaces
;; setting print_exp will give numbers in scientific notation with 'x10' & superscript exponent rather than 'e'


function number_formatter, number, format = format, flag=flag, print_exp = print_exp

  if n_elements(flag) eq 0 then flag = 2
  trm = strtrim(string(number,format=format), flag)

  ;; first check if there is a decimal point. Only remove
  ;; trailing 0's if there is a decimal point
  decimal_pos = strpos(trm, '.')
  wh_dec = where(decimal_pos gt -1, count_dec)
  if count_dec gt 0 then begin
     working_trm = trm[wh_dec]

     ;;  Find and remove any exponential.
     len = strlen(working_trm)
     exp_pos = strpos(strlowcase(working_trm), 'e')
     wh_exp = where(exp_pos gt -1, count_exp)
     exp = strarr(count_dec)
     if count_exp gt 0 then begin
        exp[wh_exp] = strmid(working_trm[wh_exp], reform(exp_pos[wh_exp], 1, count_exp), reform(len[wh_exp], 1, count_exp))
        working_trm[wh_exp] = strmid(working_trm[wh_exp], 0, reform(exp_pos[wh_exp], 1, count_exp))
        len[wh_exp] = strlen(working_trm[wh_exp])
        
        ;; cleanup exponential
        exp_num = strmid(exp[wh_exp], 2)
        exp_num = number_formatter(string(exp_num, format = '(d0)'))
        if keyword_set(print_exp) then begin
           ;; only use sign when negative
           sign_str = strmid(exp[wh_exp], 1, 1)
           wh_pos = where(sign_str eq '+', count_pos)
           if count_pos gt 0 then sign_str[wh_pos] = ''

           exp[wh_exp] = 'x10!U' + sign_str + exp_num + '!N'
        endif else exp[wh_exp] = strmid(exp[wh_exp], 0, 2) + exp_num
     endif
     
     ;;  Keep removing trailing zeros until done.
     last_char = strmid(working_trm, reform(len-1, 1, count_dec), 1)
     wh_0 = where(last_char eq '0', count_0)
     while count_0 gt 0 do begin
        working_trm[wh_0] = strmid(working_trm[wh_0], 0, reform(len[wh_0]-1, 1, count_0))
        len[wh_0] = len[wh_0] - 1
        last_char = strmid(working_trm, reform(len-1, 1, count_dec), 1)
        wh_0 = where(last_char eq '0', count_0)
     endwhile
     
     ;;  If the last character is a period, remove it as well.
     wh_period = where(last_char eq '.', count_period)
     if count_period gt 0 then begin
        working_trm[wh_period] = strmid(working_trm[wh_period], 0, reform(len[wh_period]-1, 1, count_period))
        len[wh_period] = len[wh_period]-1
     endif
   
     ;;  Restore the exponential
     if keyword_set(print_exp) then begin
        wh = where(working_trm eq '1', count)
        if count ne 0 then begin
           working_trm[wh] = ''
           exp[wh] = strmid(exp[wh], 1)
        endif
     endif
     working_trm = working_trm + exp

     ;; replace trm by working_trm where there was a decimal
     trm[wh_dec] = working_trm
  endif

  ;;  Return the trimmed string TRM.
  return, trm
end  
