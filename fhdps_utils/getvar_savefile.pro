function getvar_savefile, savefile, varname, pointer_return=pointer_return, names = names, return_size = return_size, verbose = verbose, compatibility_mode = compatibility_mode
  if file_test(savefile) eq 0 then begin
    print, 'getvar_savefile: file ' + string(savefile) + ' not found'
    return, 0
  endif
  
  savefile_obj = obj_new('idl_savefile', savefile)
  if arg_present(names) then names = savefile_obj->names()
  
  if n_elements(varname) ne 0 then begin
    if keyword_set(return_size) then begin
      size = savefile_obj->size(varname)
      obj_destroy, savefile_obj
      
      return, size
    endif else begin
      IF Keyword_Set(compatibility_mode) THEN RESTORE,savefile,/relaxed ELSE savefile_obj->Restore, varname
      obj_destroy, savefile_obj
      
      IF Keyword_Set(verbose) THEN print,"Restoring "+varname+" from file: "+file_basename(savefile,'.sav',/fold_case)
      IF Keyword_Set(pointer_return) THEN p=execute(varname+'=Ptr_new('+varname+')')
      q=execute('return,'+varname)
    endelse
  endif else begin
    obj_destroy, savefile_obj
    return, 0
  endelse
end

