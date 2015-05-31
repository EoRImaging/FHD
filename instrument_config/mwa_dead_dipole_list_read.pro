PRO mwa_dead_dipole_list_read,obs,antenna
Jdate=obs.JD0
tile_names=Strtrim((*obs.baseline_info).tile_names,2)
ant_pol_names=['X','Y']
alpha_table=['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P']

IF Jdate LT 2456529 AND Jdate GE 2456528 THEN BEGIN ; 2456528 is August 23, 2013
    dipole_filepath=filepath(obs.instrument+'_dead_dipole_list.txt',root=rootdir('FHD'),subdir='instrument_config')
    textfast,dipole_arr,/read,file_path=dipole_filepath,/string,first_line=1
ENDIF
IF N_Elements(dipole_arr) EQ 0 THEN RETURN

entries=(size(dipole_arr,/dimension))[1]
names=Strtrim(Reform(dipole_arr[0,*]),2)
dipole_name=Strtrim(Reform(dipole_arr[1,*]),2)
pol_names=Strtrim(Reform(dipole_arr[2,*]),2)
dipole_flag=StrUpCase(Strtrim(Reform(dipole_arr[3,*]),2))

FOR i=0L,entries-1 DO BEGIN
    tile_i=where(tile_names EQ names[i],n_match_tile)
    IF n_match_tile EQ 0 THEN CONTINUE
    
    dipole_i=where(dipole_name[i] EQ alpha_table,n_match_dipole)
    IF n_match_dipole EQ 0 THEN CONTINUE
    
    FOR pol_i=0,(obs.n_pol<2)-1 DO BEGIN
        pol_match=Strpos(pol_names[i],ant_pol_names[pol_i])
        IF pol_match EQ -1 THEN CONTINUE
        
        CASE dipole_flag[i] OF
            'DEAD':(*((antenna[tile_i].gain)[pol_i]))[*,dipole_i]=0.
;            'LOW':(*((antenna[tile_i].gain)[pol_i]))[*,dipole_i]=0.5
            ELSE:
        ENDCASE
        bp=1
    ENDFOR
ENDFOR

END