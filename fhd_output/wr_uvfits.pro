;+
;Copied version 1.11 of MWRFITS.PRO, but stripped unneeded code and re-written for speed in writing large files with group parameters
;
; :Author: Ian Sullivan
;-

; Add a keyword as non-destructively as possible to a FITS header
pro chk_and_upd2, header, key, value, comment, nological=nological
     compile_opt idl2,hidden


    xcomm = ""
    if n_elements(comment) gt 0 then xcomm = comment
    if n_elements(header) eq 0 then begin
      
        fxaddpar, header, key, value, xcomm
       
    endif else begin
       
        oldvalue = fxpar(header, key, count=count, comment=oldcomment)
        if (count eq 1) then begin

           qchange = 0 ; Set to 1 if either the type of variable or its
                       ; value changes.
            size1 = size(oldvalue,/type) & size2 = size(value,/type)
            if size1 NE size2 then qchange = 1 $
            else if (oldvalue ne value) then qchange = 1

            if (qchange) then begin

               if n_elements(oldcomment) gt 0 then xcomm = oldcomment[0]
               fxaddpar, header, key, value, xcomm,nological=nological
              
           endif
           
       endif else begin
           
            fxaddpar, header, key, value, xcomm,nological=nological
        endelse
       
    endelse
end

; Write a header
pro mwr_header2, lun, header

     compile_opt idl2,hidden
    ; Fill strings to at least 80 characters and then truncate.

    space = string(replicate(32b, 80))
    header = strmid(header+space, 0, 80)

    w = where(strcmp(header,"END     ",8), Nw)

    if Nw eq 0 then begin

       header = [header, strmid("END"+space,0,80)]
       
    endif else begin
        if (Nw gt 1) then begin 
           ; Get rid of extra end keywords;
           print,"MWRFITS Warning: multiple END keywords found."
           for irec=0L, n_elements(w)-2 do begin
              header[w[irec]] = strmid('COMMENT INVALID END REPLACED'+  $
                space, 0, 80)
           endfor
       endif

       ; Truncate header array at END keyword.
       header = header[0:w[n_elements(w)-1]]
    endelse

    nrec = n_elements(header)
    if nrec mod 36 ne 0 then header = [header, replicate(space,36 - nrec mod 36)]

    writeu, lun, byte(header)
end

pro mwr_groupinfix2, data, group, hdr
     compile_opt idl2,hidden

    siz = size(data)
    sizg = size(group)

    ; Check if group info is same type as data 

    if siz[siz[0]+1] ne sizg[3] then begin
        case siz[siz[0]+1] of
         1: begin
               mwr_groupscale2, 127.d0, group, hdr
               group = byte(group)
           end
         2: begin
               mwr_groupscale2, 32767.d0, group, hdr
               group = fix(group)
           end
         3: begin
               mwr_groupscale2, 2147483647.d0, group, hdr
               group = long(group)
           end
         4: group = float(group)
         5: group = double(group)
      else: begin
                print,'MWRFITS Internal error: Conversion of group data'
               return
            end
        endcase
    endif

    nrow = 1
    for i=1, siz[0]-1 do begin
        nrow = nrow*siz[i]
    endfor


;<<<THIS LOOKS LIKE IT IS PROBABLY THE SECTION THAT SLOWS EVERYTHING DOWN!!!>>>

    data = reform(data, siz[siz[0]+2])
    for i=0L, siz[siz[0]] - 1 do begin
        if i eq 0 then begin
            gdata = group[*,0]
           gdata = reform(gdata)
            tdata = [ gdata , data[0:nrow-1]]
        endif else begin
            start = nrow*i
           fin = start+nrow-1
           gdata = group[*,i]
            tdata = [tdata, gdata ,data[start:fin]]
       endelse
    endfor

    data = temporary(tdata)
end

; If an array is being scaled to integer type, then
; check to see if the group parameters will exceed the maximum
; values allowed.  If so scale them and update the header.
pro mwr_groupscale2, maxval, group, hdr
     compile_opt idl2,hidden

    sz = size(group)
    for i=0L, sz[1]-1 do begin
         pmax = max(abs(group[i,*]))
         if (pmax gt maxval) then begin
             ratio = pmax/maxval
            psc = 'PSCAL'+strcompress(string(i+1),/remo)
            currat = fxpar(hdr, psc)
            if (currat ne 0) then begin
                fxaddpar, hdr, psc, currat*ratio, 'Scaling overriden by MWRFITS'
            endif else begin
                fxaddpar, hdr, psc, ratio, ' Scaling added by MWRFITS'
            endelse
             group[i,*] = group[i,*]/ratio
         endif
    endfor
end

; Scale parameters for GROUPed data.
pro mwr_pscale2, grp, header, pscale=pscale, pzero=pzero, ptype=ptype
     compile_opt idl2,hidden
    

; This function assumes group is a 2-d array.

    if ~keyword_set(pscale) && ~keyword_set(pzero) then return

    sizg=size(grp)
    if ~keyword_set(pscale) then begin
        pscale = dblarr(sizg[1])
        pscale[*] = 1.
    endif
    
    if ~keyword_set(pzero) then begin
        pzero = dblarr(sizg[1])
        pzero[*] = 0.
    endif

    w = where(pzero eq 0.d0)

    if w[0] ne 0 then begin
        print, 'MWRFITS  Warning: PSCALE value of 0 found, set to 1.'
        pscale[w] = 1.d0
    endif
    
    FOR i=0L, sizg[1]-1 DO BEGIN
        IF Keyword_Set(ptype) THEN BEGIN
            key= 'PTYPE' + strcompress(string(i+1),/remo)
            chk_and_upd2, header, key, ptype[i]
        ENDIF
        IF Keyword_Set(pscale) THEN BEGIN
            key= 'PSCAL' + strcompress(string(i+1),/remo)
            chk_and_upd2, header, key, pscale[i]
        ENDIF
        IF Keyword_Set(pzero) THEN BEGIN
            key= 'PZERO' + strcompress(string(i+1),/remo)
            chk_and_upd2, header, key, pzero[i]
        ENDIF
    ENDFOR

;    for i=0L, sizg[1]-1 do begin
;        grp[i,*] = grp[i,*]/pscale[i] - pzero[i]
;    endfor

end

pro wr_uvfits, xinput, file, header,              $
       create=create,                          $
       null=null,                              $
       group=group,                            $
       pscale=pscale, pzero=pzero, ptype=ptype,             $                       $
       silent=silent,                          $
       status = status


    ; Check required keywords.
    compile_opt idl2
    status = -1                     ;Status changes to 0 upon completion


    ; Save the data into an array/structure that we can modify.
 
    if n_elements(xinput) gt 0 then data = xinput

;    on_ioerror, open_error

    ; Open the input file.    If it exists, and the /CREATE keyword is not 
    ; specified, then we append to to the existing file.
     ;

    if  ~keyword_set(create) && file_test(file) then begin
        openu, lun, file, /get_lun, /append,/swap_if_little
        if ~keyword_set(silent) then $
        message,/inf,'Appending FITS extension to file ' + file
        bof = 0
    endif else begin 
        openw, lun, file, /get_lun, /swap_if_little
         bof = 1
    endelse      
    on_ioerror, null


    siz = size(data) 
    type = siz[siz[0] + 1]

    bitpixes=[8,8,16,32,-32,-64,-32,0,0,-64,0,0,16,32,64,64]


    if bof then begin
        chk_and_upd2, hdr, 'SIMPLE', 'T','Primary Header created by MWRFITS v'+mwr_version()
        chk_and_upd2, hdr, 'BITPIX', bitpixes[type]
        chk_and_upd2, hdr, 'NAXIS', siz[0]
        chk_and_upd2, hdr, 'EXTEND', 'T', 'Extensions may be present'
    endif else begin
        chk_and_upd2, hdr, 'XTENSION', 'IMAGE','Image Extension created by MWRFITS v'+mwr_version()
        chk_and_upd2, hdr, 'BITPIX', bitpixes[type]
        chk_and_upd2, hdr, 'NAXIS', siz[0]
        chk_and_upd2, hdr, 'PCOUNT', 0
        chk_and_upd2, hdr, 'GCOUNT', 1
    endelse


    if keyword_set(group) then begin
        group_offset = 1
    endif else group_offset = 0

    if keyword_set(group) then begin
       chk_and_upd2, hdr, 'NAXIS1', 1  ;0 ;CHANGED for compatibility with most fits readers
    endif

    for i=1L, siz[0]-group_offset do begin
        chk_and_upd2, hdr, 'NAXIS'+strcompress(string(i+group_offset),/remo), siz[i]
    endfor


    if keyword_set(group) then begin
        chk_and_upd2, hdr, 'GROUPS', 'T'
        sizg = size(group)
        if sizg[0] ne 2 then begin
            print,'MWRFITS Error: Group data is not 2-d array'
           return
        endif
        if sizg[2] ne siz[siz[0]] then begin
            print,'MWRFITS Error: Group data has wrong number of rows'
           return
        endif
        chk_and_upd2,hdr,  'PCOUNT', sizg[1]
        chk_and_upd2, hdr, 'GCOUNT', siz[siz[0]]
    endif
   
    ; If grouped data scale the group parameters.
    if keyword_set(group) then mwr_pscale2, group, hdr, pscale=pscale, pzero=pzero, ptype=ptype
    
    
    bytpix=abs(bitpixes[siz[siz[0]+1]])/8             ; Number of bytes per pixel.
    npixel = n_elements(data) + n_elements(group)     ; Number of pixels.

    if keyword_set(group) then mwr_groupinfix2, data, group, hdr

    ; Write the FITS header
    mwr_header2, lun, hdr

    ; This is all we need to do if data is undefined.
    if (n_elements(data) eq 0) || (siz[0] eq 0) then return

    ; Write the data.
    writeu, lun, data

    nbytes = bytpix*npixel
    filler = 2880 - nbytes mod 2880
    if filler eq 2880 then filler = 0
  
    ; Write any needed filler.
    if filler gt 0 then writeu, lun, replicate(0B,filler)
    
    free_lun, lun
    status=0
    return
    
    ; Handle error in opening file.
;  open_error:
;    on_ioerror, null
;    print, 'MWRFITS Error: Cannot open output: ', file
;     print,!ERROR_STATE.SYS_MSG
;    if n_elements(lun) gt 0 then free_lun, lun
;    
;    return
end