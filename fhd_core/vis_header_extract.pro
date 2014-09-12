FUNCTION vis_header_extract,header,header2=header2, params=params
compile_opt idl2,strictarrsubs  

pol_dim=2
freq_dim=4
real_index=0
imaginary_index=1
flag_index=2

n_extensions=sxpar(header,'nextend')
naxis=sxpar(header,'naxis') ;6
n_grp_params=sxpar(header,'pcount') ;5
nbaselines=sxpar(header,'gcount') ;variable, based on length of observation
n_complex=sxpar(header,'naxis2') ;3 columns are amplitude, phase (degrees), weights
n_polarizations=sxpar(header,'naxis3') ;4 columns are xx, yy, xy, yx
n_frequencies=sxpar(header,'naxis4') ;768
freq_ref=sxpar(header,'crval4') ;1.5424E8
freq_width=sxpar(header,'cdelt4') ;40000 
freq_ref_i=sxpar(header,'crpix4') ;368
date_obs=sxpar(header,'date-obs')

n_fields=sxpar(header,'tfields') ;12

IF Keyword_Set(header2) THEN BEGIN
  Jdate0=sxpar(header,'pzero5')-0.5 ;2455463.5 ;DATE-OBS

  obsra=sxpar(header,'obsra')
  obsdec=sxpar(header,'obsdec')
  n_grp_params=sxpar(header2,'pcount')
  
  param_list=Strarr(n_grp_params)
  ptype_list=String(format='("PTYPE",I1)',indgen(n_grp_params)+1)
  FOR pi=0,n_grp_params-1 DO param_list[pi]=StrTrim(sxpar(header2,ptype_list[pi]),2)
  
  n_tile=Long(sxpar(header,'INSTRUME'))
ENDIF ELSE BEGIN
  n_grp_params=sxpar(header,'pcount')
  param_list=Strarr(n_grp_params)
  ptype_list=String(format='("PTYPE",I1)',indgen(n_grp_params)+1)
  FOR pi=0,n_grp_params-1 DO param_list[pi]=StrTrim(sxpar(header,ptype_list[pi]),2)
  
  year = strmid(date_obs, 0, 4)
  month = strmid(date_obs, 5, 2)
  day = strmid(date_obs, 8, 2)
  hour = strmid(date_obs, 11, 2)
  minute = strmid(date_obs, 14, 2)
  second = strmid(date_obs, 17, 2)
  Jdate0 = Floor(julday(month, day, year, hour, minute, second))
 
  param_names = strlowcase(strtrim(sxpar(header, 'CTYPE*'), 2))
  wh_ra = where(param_names eq 'ra', count_ra)
  if count_ra eq 1 then ra_cnum = wh_ra+1 else message, 'RA CTYPE not found'
  wh_dec = where(param_names eq 'dec', count_dec)
  if count_dec eq 1 then dec_cnum = wh_dec+1 else message, 'DEC CTYPE not found'
 
  obsra=sxpar(header,'CRVAL' + strn(ra_cnum[0]))
  obsdec=sxpar(header,'CRVAL' + strn(dec_cnum[0]))  
ENDELSE

baseline_i=(where(Strmatch(param_list,'BASELINE'),found_baseline))[0]
uu_i=(where(Strmatch(param_list,'UU'),found_uu))[0]
vv_i=(where(Strmatch(param_list,'VV'),found_vv))[0]
ww_i=(where(Strmatch(param_list,'WW'),found_ww))[0]
baseline_i=(where(Strmatch(param_list,'BASELINE'),found_baseline))[0]
date_i=Max(where(Strmatch(param_list,'DATE'),found_date))
Jdate_extract=sxpar(header,String(format='("PZERO",I1)',date_i+1),count=found_jd0)
IF Keyword_Set(found_jd0) THEN IF Jdate_extract GT 2.4E6 THEN Jdate0=Jdate_extract

;; get number of tiles from number of baselines.
;256 tile upper limit is hard-coded in CASA format
;these tile numbers have been verified to be correct
baseline_arr=reform(params[baseline_i,*]) 
tile_A1=Long(Floor(baseline_arr/256)) ;tile numbers start from 1
tile_B1=Long(Fix(baseline_arr mod 256))
hist_A1=histogram(tile_A1,min=0,max=256,/binsize)
hist_B1=histogram(tile_B1,min=0,max=256,/binsize)
hist_AB=hist_A1+hist_B1
tile_nums=where(hist_AB,n_tile)
;tile_A=Long(Floor(baseline_arr/256)) ;tile numbers start from 1
;n_tile=n_elements(uniq(tile_A[sort(tile_A)]))

grp_row_size=n_complex*n_polarizations*n_frequencies*nbaselines

struct={n_params:n_grp_params,nbaselines:nbaselines,n_tile:n_tile,n_pol:n_polarizations,n_freq:n_frequencies,$
    freq_ref:freq_ref,freq_width:freq_width,freq_ref_i:freq_ref_i,obsra:obsra,obsdec:obsdec,date:date_obs,$
    uu_i:uu_i,vv_i:vv_i,ww_i:ww_i,baseline_i:baseline_i,date_i:date_i,jd0:Jdate0,$
    pol_dim:pol_dim,freq_dim:freq_dim,real_index:real_index,imaginary_index:imaginary_index,flag_index:flag_index}
RETURN,struct
END