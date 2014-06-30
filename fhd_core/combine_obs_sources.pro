PRO combine_obs_sources,file_list,calibration,source_list,restore_last=restore_last,output_path=output_path,_Extra=extra

except=!except
!except=0 
heap_gc

ref_i=0.

save_path=output_path+'_sources.sav'

StoN=2. ;minimum signal to noise threshold 
min_detect=3. ;minimum number of observations a given source must be detected in
detect_radius=3.0 ;maximum radius in pixels to consider sources as matches. Converted to degrees at zenith

ns_use=0.

IF file_test(save_path) EQ 0 THEN restore_last=0
IF Keyword_Set(restore_last) THEN BEGIN
    restore,filename=save_path 
    n_files=N_Elements(file_list)
    RETURN
ENDIF

n_files=N_Elements(file_list)
IF n_files LT 2 THEN BEGIN
    RETURN
ENDIF
source_array=Ptrarr(n_files,/allocate)

n_src=fltarr(n_files)
src_i=Ptrarr(n_files,/allocate)
fi_c=-1
FOR fi=0,n_files-1 DO BEGIN
    file_path_fhd=file_list[fi]
    IF (file_test(file_path_fhd+'_obs.sav') EQ 0) OR (file_test(file_path_fhd+'_fhd.sav') EQ 0) THEN CONTINUE ELSE fi_c+=1
    
    sa=getvar_savefile(file_path_fhd+'_fhd.sav','source_array')
    obs=getvar_savefile(file_path_fhd+'_obs.sav','obs')
    IF fi_c EQ 0 THEN obs_arr=Replicate(obs,n_files)
    obs_arr[fi]=obs
    
    src_i0=where(sa.ston GE StoN,n_src0)
    n_src[fi]=n_src0
    *src_i[fi]=src_i0
    *source_array[fi]=sa
ENDFOR
IF fi_c LE 1 THEN RETURN
degpix=Median(obs_arr.degpix)
n_pol=Min(obs_arr.n_pol)

beam_arr=Ptrarr(n_files,n_pol)
FOR fi=0,n_files-1 DO BEGIN
    file_path_fhd=file_list[fi]
    IF (file_test(file_path_fhd+'_obs.sav') EQ 0) OR (file_test(file_path_fhd+'_fhd.sav') EQ 0) THEN CONTINUE
    beam_arr_single=getvar_savefile(file_path_fhd+'_fhd.sav','beam_base')
    beam_arr[fi,*]=beam_arr_single[0:n_pol-1]
ENDFOR

ns=Total(n_src)
ns_i=[0,Total(n_src,/cumulative)]
sa_x=fltarr(ns)
sa_y=fltarr(ns)
sa_ra=fltarr(ns)
sa_dec=fltarr(ns)
sa_I=fltarr(ns)
sa_bin=fltarr(ns)
sa_id=fltarr(ns)
sa_ston=fltarr(ns)
sa_beam=fltarr(ns)
sa_radius=fltarr(ns)
sa_extend=fltarr(ns)

FOR fi=0,n_files-1 DO BEGIN
    IF n_src[fi] EQ 0 THEN CONTINUE
    
    IF n_pol GT 1 THEN beam_avg=Sqrt(((*beam_arr[fi,0])^2.+(*beam_arr[fi,1])^2.)/2.) $
        ELSE beam_avg=*beam_arr[fi,0]
    sa=*source_array[fi]
    sa=sa[*src_i[fi]]
    sa_inds=lindgen(n_src[fi])+ns_i[fi]
    
    pix_i=Floor(sa.x)+Floor(sa.y)*obs_arr[fi].dimension
    ;replacing ns_i[fi]:ns_i[fi+1]-1 with sa_inds
    sa_x[sa_inds]=sa.x
    sa_y[sa_inds]=sa.y
    sa_ra[sa_inds]=sa.ra
    sa_dec[sa_inds]=sa.dec
    sa_I[sa_inds]=sa.flux.I
    sa_bin[sa_inds]=fi
    sa_id[sa_inds]=sa.id
    sa_ston[sa_inds]=sa.ston
    sa_radius[sa_inds]=angle_difference(obs_arr[fi].obsdec,obs_arr[fi].obsra,sa.dec,sa.ra,/degree)
    sa_beam[sa_inds]=beam_avg[pix_i]
    sa_extend[sa_inds]=Ptr_valid(sa.extend)
ENDFOR

group_id=fltarr(ns)-1
g_id=0
radius=detect_radius*degpix
FOR si=0L,ns-1 DO BEGIN 
    IF group_id[si] GE 0 THEN CONTINUE ;skip sources already grouped
    IF sa_I[si] EQ 0 THEN CONTINUE
    si_use=where(group_id EQ -1,n_use)  
    IF n_use EQ 0 THEN CONTINUE
    dx=sa_ra[si]-sa_ra[si_use]
    dy=sa_dec[si]-sa_dec[si_use]
    dr=sqrt(dx^2.+dy^2.)
    group_i=where(dr LE radius,n_group) ;guaranteed at least one since si is included in si_use ;NOT always true! 
    IF n_group EQ 0 THEN CONTINUE
    group_id[si_use[group_i]]=g_id
    g_id+=1
ENDFOR

hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
ng=max(group_id)


source_list_base={id:-1,ra:0.,dec:0.,ston_max:0.,flux_max:0.,n_detect:0.,extend:0,sources:Ptr_new()}

source_sub_base={filename:'XXX',extend:0.,radius:0.,alpha:0.,ston:0.,flux:0.,beam:0.,fi:-1.,fii:-1.,x:-1.,y:-1.,ra:-1.,dec:-1.}

source_list=Replicate(source_list_base,ng)
FOR gi=0L,ng-1 DO BEGIN
    si_g=gri[gri[gi]:gri[gi+1]-1]; guaranteed at least one source per group
    ston1=sa_ston[si_g]
    ston_max=Max(ston1,max_i)
    source_list[gi].id=gi
    source_list[gi].ra=sa_ra[si_g[max_i]]
    source_list[gi].dec=sa_dec[si_g[max_i]]
    source_list[gi].ston_max=sa_ston[si_g[max_i]]
    source_list[gi].flux_max=sa_I[si_g[max_i]]
    
    bin_list=sa_bin[si_g]
    h_id=histogram(bin_list,min=0,binsize=1,reverse_ind=rgi)
    uniq_i=where(h_id,n_gs)
    si_g_use=lonarr(n_gs)
    source_list[gi].n_detect=n_gs
    
    FOR gs_i=0,n_gs-1 DO BEGIN
        sub_max=Max(sa_I[si_g[rgi[rgi[uniq_i[gs_i]]:rgi[uniq_i[gs_i]+1]-1]]],max_i)
        si_g_use[gs_i]=(si_g[rgi[rgi[uniq_i[gs_i]]:rgi[uniq_i[gs_i]+1]-1]])[max_i]
    ENDFOR
    
    IF N_Elements(where(sa_ston[si_g_use] GE StoN)) GE min_detect THEN ns_use+=1.
    
    source_sub=replicate(source_sub_base,n_gs)
    source_sub.ston=sa_ston[si_g_use]
    source_sub.flux=sa_I[si_g_use]
    source_sub.beam=sa_beam[si_g_use]
    source_sub.fi=sa_bin[si_g_use]
    source_sub.fii=sa_id[si_g_use]
    source_sub.x=sa_x[si_g_use]
    source_sub.y=sa_y[si_g_use]
    source_sub.ra=sa_ra[si_g_use]
    source_sub.dec=sa_dec[si_g_use]
    source_sub.filename=file_basename(file_list[source_sub.fi])
    source_sub.extend=sa_extend[si_g_use]
    source_sub.radius=sa_radius[si_g_use]
    source_list[gi].extend=median(sa_extend[si_g_use])
    
    source_list[gi].sources=Ptr_new(source_sub)
ENDFOR

SAVE,source_list,filename=save_path,/compress
print,Strn(ns_use)+" sources detected in at least "+Strn(min_detect)+" observations."

END
