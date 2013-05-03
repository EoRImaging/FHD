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
source_array2=Ptrarr(n_files,/allocate)
beam_arr=Ptrarr(n_files,2)

n_src=fltarr(n_files)
src_i=Ptrarr(n_files,/allocate)
fi_c=-1
FOR fi=0,n_files-1 DO BEGIN
    file_path=file_list[fi]
    IF (file_test(file_path+'_obs.sav') EQ 0) OR (file_test(file_path+'_fhd.sav') EQ 0) THEN CONTINUE ELSE fi_c+=1
    sa=getvar_savefile(file_path+'_fhd.sav','source_array')
    beam_arr[fi,*]=getvar_savefile(file_path+'_fhd.sav','beam_base')
;    sa_path=filepath(file_basename(file_path),root=file_dirname(file_path),subdir='export')+'_source_list'
    restore,file_path+'_obs.sav'
    IF fi_c EQ 0 THEN obs_arr=Replicate(obs,n_files)
    obs_arr[fi]=obs
    
;    textfast,sa,/read,file_path=sa_path,first_line=1
    
    src_i0=where(sa.ston GE StoN,n_src0)
    n_src[fi]+=n_src0
    *src_i[fi]=src_i0
    *source_array2[fi]=sa
ENDFOR
degpix=Median(obs_arr.degpix)

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
    
    sa=*source_array2[fi]
    sa=sa[*src_i[fi]]
    sa_x[ns_i[fi]:ns_i[fi+1]-1]=sa.x
    sa_y[ns_i[fi]:ns_i[fi+1]-1]=sa.y
    sa_ra[ns_i[fi]:ns_i[fi+1]-1]=sa.ra
    sa_dec[ns_i[fi]:ns_i[fi+1]-1]=sa.dec
    sa_I[ns_i[fi]:ns_i[fi+1]-1]=sa.flux.I
    sa_bin[ns_i[fi]:ns_i[fi+1]-1]=fi
    sa_id[ns_i[fi]:ns_i[fi+1]-1]=sa.id
    sa_ston[ns_i[fi]:ns_i[fi+1]-1]=sa.ston
    sa_radius[ns_i[fi]:ns_i[fi+1]-1]=angle_difference(obs_arr[fi].obsdec,obs_arr[fi].obsra,sa.dec,sa.ra,/degree)
    sa_beam[ns_i[fi]:ns_i[fi+1]-1]=(*beam_arr[fi])[sa.x,sa.y]
    sa_extend[ns_i[fi]:ns_i[fi+1]-1]=Ptr_valid(sa.extend)
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

source_sub_base={filename:'XXX',extend:0.,radius:0.,alpha:0.,ston:0.,flux:0.,beam:0.,fi:-1.,fii:-1.,x:-1.,y:-1.}

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
    source_sub.filename=file_basename(file_list[source_sub.fi])
    source_sub.extend=sa_extend[si_g_use]
    source_sub.radius=sa_radius[si_g_use]
    source_list[gi].extend=median(sa_extend[si_g_use])
    
    source_list[gi].sources=Ptr_new(source_sub)
ENDFOR

save,source_list,filename=save_path
print,Strn(ns_use)+" sources detected in at least "+Strn(min_detect)+" observations."

END
;source_matrix=fltarr(n_files,ng)
;FOR gi=0L,ng-1 DO source_matrix[(*source_list[gi].sources).fi,gi]=(*source_list[gi].sources).flux
;beam_matrix=fltarr(n_files,ng)
;FOR gi=0L,ng-1 DO beam_matrix[(*source_list[gi].sources).fi,gi]=(*source_list[gi].sources).beam
;
;cal_matrix=fltarr(n_files,n_files)
;sigma_matrix=fltarr(n_files,n_files)
;FOR fi=0,n_files-1 DO BEGIN
;    si_use=where(source_matrix[fi,*] AND (source_list.n_detect GE min_detect),n_use)
;    IF n_use EQ 0 THEN CONTINUE
;    
;    sm_use=source_matrix[*,si_use]
;    FOR si=0,n_use-1 DO sm_use[*,si]/=sm_use[fi,si]
;    
;    cal_single=fltarr(n_files)
;    sigma_single=fltarr(n_files)
;    FOR fi2=0,n_files-1 DO BEGIN
;        IF fi2 EQ fi THEN BEGIN cal_single[fi2]=1 & CONTINUE & ENDIF
;        si_use2=where(sm_use[fi2,*],n_use2)
;        IF n_use2 EQ 0 THEN CONTINUE
;        
;        vals=sm_use[fi2,si_use2]
;        beam_vals=beam_matrix[fi2,si_use[si_use2]]
;        
;        IF n_use2 LT 3 THEN BEGIN 
;            cal_single[fi2]=Mean(vals) 
;            sigma_single[fi2]=1./Min(beam_vals)
;            CONTINUE 
;        ENDIF
;        
;        
;        sigma=Sqrt(Total((ALog(vals)-median(ALog(vals)))^2.)/n_use2)
;        si_use3=where(Abs(ALog(vals)-Median(ALog(vals))) LE 2.*sigma,n_use3) ;this should ALWAYS return at least one valid index
;;            sigma_single[fi2]=Exp(sigma)-1.
;        sigma_single[fi2]=Sqrt(1./Total(beam_vals[si_use3]^2.))
;        cal_single[fi2]=Median(vals[si_use3],/even)
;        
;    ENDFOR
;    cal_matrix[fi,*]=cal_single
;    sigma_matrix[fi,*]=sigma_single
;    
;ENDFOR
;
;sig_test=fltarr(n_files)
;FOR i=0,n_files-1 DO sig_test[i]=Stddev(sigma_matrix[i,*])
;file_use_i=where(sig_test,nf_use)
;
;calibration=fltarr(n_files) & calibration[0]=1.
;error=fltarr(n_files)
;reference_list=Ptrarr(n_files,/allocate)
;*reference_list[0]=[0]
;err_max=Max(sigma_matrix)
;file_i_list=lindgen(n_files)
;FOR fi=1,nf_use-1 DO BEGIN
;    ref=sigma_matrix[0,fi]
;    IF ref LE 0 THEN ref=max(sigma_matrix)
;    inds=where((sigma_matrix[0,1:*] LE ref) AND (cal_matrix[0,1:*] GT 0),ni)+1
;    IF ni LE 1 THEN BEGIN
;        calibration[file_use_i[fi]]=cal_matrix[0,fi]
;        error[file_use_i[fi]]=ref
;        CONTINUE
;    ENDIF
;
;    ref_list=Ptrarr(ni,/allocate)
;    err_tot=fltarr(ni)
;    FOR ni_i=0,ni-1 DO BEGIN
;        
;        err1=sigma_matrix[0,inds[ni_i]]
;        
;    ;        *ref_list[ni_i]=[inds[ni_i]]
;    ;        err_tot[ni_i]=err1
;        
;        ref1=Sqrt((err1)^2.+(sigma_matrix[inds[ni_i],fi])^2.)
;        IF ref1 GT 0 THEN IF ref1 LT ref THEN BEGIN
;            ref=ref1
;        ENDIF
;        
;        inds1=where((Sqrt((sigma_matrix[inds[ni_i],*])^2.+err1^2.) LE ref) $
;            AND (cal_matrix[inds[ni_i],*] GT 0),ni1)
;        
;        IF ni1 EQ 0 THEN BEGIN
;            err_tot[ni_i]=err_max
;            *ref_list[ni_i]=[inds[ni_i],fi]
;            CONTINUE
;        ENDIF 
;        
;        err_tot2=fltarr(ni1)
;        FOR ni1_i=0,ni1-1 DO $
;            IF sigma_matrix[inds1[ni1_i],fi] EQ 0 THEN err_tot2[ni1_i]=err_max ELSE $
;                err_tot2[ni1_i]=Sqrt(err1^2.+sigma_matrix[inds[ni_i],inds1[ni1_i]]^2.+sigma_matrix[inds1[ni1_i],fi]^2.)
;        
;        err_tot[ni_i]=Min(err_tot2,ni2_i)
;        *ref_list[ni_i]=[inds[ni_i],inds1[ni2_i],fi]
;        
;    ENDFOR 
;    error[file_use_i[fi]]=Min(err_tot,min_i)
;    *reference_list[fi]=*ref_list[min_i]
;    rl=*ref_list[min_i]
;    
;    cal_single=1.
;    SWITCH N_Elements(rl) OF
;        3:cal_single*=cal_matrix[rl[1],rl[2]]
;        2:cal_single*=cal_matrix[rl[0],rl[1]]
;        1:cal_single*=cal_matrix[0,rl[0]]
;    ENDSWITCH
;    
;    calibration[file_use_i[fi]]=cal_single
;    
;    Ptr_free,ref_list
;ENDFOR
;
;FOR gi=0L,ng-1 DO BEGIN
;    ns1=source_list[gi].n_detect
;    FOR si=0L,ns1-1 DO BEGIN
;        cal_use=calibration[(*source_list[gi].sources)[si].fi]
;        IF cal_use EQ 0 THEN cal_use=-1. 
;        (*source_list[gi].sources)[si].flux/=cal_use
;    ENDFOR
;ENDFOR 
;save,filename=save_path,source_list,source_matrix,sigma_matrix,cal_matrix,calibration,file_list,degpix
;save,source_list,filename=save_path
;print,Strn(ns_use)+" sources detected in at least "+Strn(min_detect)+" observations."
;
;END