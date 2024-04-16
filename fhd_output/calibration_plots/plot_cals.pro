pro plot_cals,cal,obs,cal_res=cal_res,cal_auto=cal_auto,vis_baseline_hist=vis_baseline_hist,file_path_base=file_path_base,no_ps=no_ps,$
    cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize
; Make plot of the cal solutions, save to png
; cgPS_Open/cgPS_Close write .ps first, then converts to png. Supply .png
; filename to automatically overwrite .ps.


IF Keyword_Set(cal_res) THEN res_gain_arr=cal_res.gain ELSE IF tag_exist(cal,'gain_residual') THEN $
    IF Min(Ptr_valid(cal.gain_residual)) THEN res_gain_arr=cal.gain_residual
tile_names = cal.tile_names
n_tiles=obs.n_tile
n_pol=cal.n_pol
pol_names=Strmid(obs.pol_names[0:1],0,1)
obs2=*obs.baseline_info
tile_use=obs2.tile_use
freq_use=obs2.freq_use
freq_i_use=where(freq_use,nf_use) & IF nf_use EQ 0 THEN freq_i_use=lindgen(obs.n_freq)
IF not Keyword_Set(cal_plot_charsize) THEN cal_plot_charsize=0.5
IF not Keyword_Set(cal_plot_symsize) THEN cal_plot_symsize=1
IF not Keyword_Set(cal_plot_resize) THEN cal_plot_resize=100.
no_ps=1
IF Keyword_Set(no_ps) THEN ext_name='.png' ELSE ext_name='.ps'

page_tile_max = 128.
page_num = ceil(n_tiles/page_tile_max)

;Make multiple plot pages if there are a large number of tiles
for file_i=0, page_num-1 do begin
  
    if page_num GT 1 then pagename='_page' + strtrim(file_i,2) else pagename=''

    phase_filename=file_path_base+'_cal_phase'+pagename+ext_name
    amp_filename=file_path_base+'_cal_amp'+pagename+ext_name
    auto_filename=file_path_base+'_cal_autocorr'+pagename+ext_name
    res_filename_amp=file_path_base+'_cal_residual_amp'+pagename+ext_name
    res_filename_phase=file_path_base+'_cal_residual_phase'+pagename+ext_name
    res_real_im_filename0=file_path_base+'_cal_residual_realVsIm'+pol_names[0]+pagename+ext_name
    res_real_im_filename1=file_path_base+'_cal_residual_realVsIm'+pol_names[1]+pagename+ext_name
    orig_amp_filename=file_path_base+'_cal_unfit_amp'+pagename+ext_name
    orig_phase_filename=file_path_base+'_cal_unfit_phase'+pagename+ext_name
    vis_hist_filename=file_path_base+'_cal_hist'+ext_name
    IF file_test(file_dirname(file_path_base),/directory) EQ 0 THEN file_mkdir,file_dirname(file_path_base)

    gains0 = *cal.gain[0] ; save on typing
    IF n_pol GT 1 THEN gains1 = *cal.gain[1]
    gains0=gains0[freq_i_use,*]
    IF n_pol GT 1 THEN gains1=gains1[freq_i_use,*]
    
    if page_num GT 1 then begin
        gains0=gains0[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains0))[2]-1]
        IF n_pol GT 1 THEN gains1=gains1[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains1))[2]-1]
    endif
    
    freq=cal.freq[freq_i_use]
    freq=freq/10^6. ; in MHz

    plot_pos=calculate_plot_positions(.8, nplots=page_tile_max, /no_colorbar, ncol=17, nrow=ceil(page_tile_max/16.)+1, plot_margin = [.05, .05, .02, .25]); use 1 extra row/column to leave room for title and axes
    plot_pos=reform(plot_pos.plot_pos,17,ceil(page_tile_max/16.)+1,4)
    ; Now remove unwanted row/column
    plot_pos = plot_pos[1:*,*,*]
    plot_pos = plot_pos[*,1:*,*]
    plot_pos = reform(plot_pos,page_tile_max,4)
    ; Shift a half a width over and up
    width = plot_pos[1,0]-plot_pos[0,0]
    height = abs(plot_pos[16,1]-plot_pos[0,1])
    plot_pos[*,0] -= width/2
    plot_pos[*,1] += height/2
    plot_pos[*,2] -= width/2
    plot_pos[*,3] += height/2

    n_baselines=obs.nbaselines
    tile_A=obs2.tile_A[0:n_baselines-1]
    tile_B=obs2.tile_B[0:n_baselines-1]
    tile_exist=(histogram(tile_A,min=1,/bin,max=(max(tile_A)>max(tile_B)))+histogram(tile_B,min=1,/bin,max=(max(tile_A)>max(tile_B))))<1

    if page_num GT 1 then begin
        tile_names=cal.tile_names[file_i*page_tile_max:(file_i+1)*page_tile_max<(size(cal.tile_names))[1]-1]
        tile_use=obs2.tile_use[file_i*page_tile_max:(file_i+1)*page_tile_max<(size(obs2.tile_use))[1]-1]
        tile_exist=tile_exist[file_i*page_tile_max:(file_i+1)*page_tile_max<(size(tile_exist))[1]-1]
    endif

    ;plot fitted phase solutions
    plot_cals_sub,freq,gains0,gains1,filename=phase_filename,/phase,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
        obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize

    ;plot fitted amplitude solutions
    plot_cals_sub,freq,gains0,gains1,filename=amp_filename,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
        obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange
    
    IF Keyword_Set(cal_auto) THEN BEGIN
        gains0_auto = (*cal_auto.gain[0])[freq_i_use,*] ; save on typing
        IF n_pol GT 1 THEN gains1_auto = (*cal_auto.gain[1])[freq_i_use,*]
        if page_num GT 1 then begin
            gains0_auto=gains0_auto[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains0_auto))[2]-1]
            IF n_pol GT 1 THEN gains1_auto=gains1_auto[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains1_auto))[2]-1]
        endif
        plot_cals_sub,freq,gains0_auto,gains1_auto,filename=auto_filename,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
            obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange
    ENDIF

    IF Keyword_Set(res_gain_arr) THEN BEGIN
        gains0_res=*res_gain_arr[0]
        gains0_res=gains0_res[freq_i_use,*]
        
        if page_num GT 1 then gains0_res=gains0_res[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains0_res))[2]-1]
    
        gains0_orig=gains0_res+gains0
        gains0_res_abs=Abs(gains0_orig)-Abs(gains0)
        IF n_pol GT 1 THEN BEGIN    
            gains1_res=*res_gain_arr[1]
            gains1_res=gains1_res[freq_i_use,*]
            if page_num GT 1 then gains1_res=gains1_res[*,file_i*page_tile_max:(file_i+1)*page_tile_max<(size(gains1_res))[2]-1]
            gains1_orig=gains1_res+gains1
            gains1_res_abs=Abs(gains1_orig)-Abs(gains1)
            max_amp_res = mean(abs([gains0_res_abs,gains1_res_abs]),/NAN) + 3*stddev(abs([gains0_res_abs,gains1_res_abs]),/NAN)
        ENDIF ELSE max_amp_res = mean(abs(gains0_res_abs),/NAN) + 3*stddev(abs(gains0_res_abs),/NAN)
    
        ;plot amplitude residuals
        IF max_amp_res GT 0 THEN BEGIN
            ; plot unfit phase
            plot_cals_sub,freq,gains0_orig,gains1_orig,filename=orig_phase_filename,/phase,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
            obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize
            ; plot unfit amp
            plot_cals_sub,freq,gains0_orig,gains1_orig,filename=orig_amp_filename,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
                obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange
            ; plot phase residual
            plot_cals_sub,freq,gains0_res,gains1_res,filename=res_filename_phase,/phase,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
              obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize
            ; plot abs residual
            plot_cals_sub,freq,gains0_res_abs,gains1_res_abs,filename=res_filename_amp,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
                obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange_res
            ; plot real im residual
            plot_cals_sub,freq,gains0_res,filename=res_real_im_filename0,/real_vs_imaginary,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
                obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange_res
            IF n_pol GT 1 THEN plot_cals_sub,freq,gains1_res,filename=res_real_im_filename1,/real_vs_imaginary,tile_A=tile_A,tile_B=tile_B,tile_use=tile_use,tile_exist=tile_exist,tile_names=tile_names,$
                obsname=obs.obsname,plot_pos=plot_pos,cal_plot_charsize=cal_plot_charsize,cal_plot_symsize=cal_plot_symsize,cal_plot_resize=cal_plot_resize,yrange=yrange_res
        ENDIF
    ENDIF
    
ENDFOR

IF size(vis_baseline_hist,/type) EQ 8 THEN BEGIN
   ratio=vis_baseline_hist.vis_res_ratio_mean ; just save some typing
   sigma=vis_baseline_hist.vis_res_sigma
   base_len=vis_baseline_hist.baseline_length

   cgPS_Open,filename=vis_hist_filename,/quiet,/nomatch
   !p.multi=[0,2,1]
   FOR pol=0,1 DO BEGIN
      cgplot,base_len,ratio[pol,*],color='red',/xlog,yrange=[0,max(ratio+sigma)]
      cgoplot,base_len,ratio[pol,*]+sigma[pol,*],linestyle=2
      cgoplot,base_len,ratio[pol,*]-sigma[pol,*],linestyle=2
   ENDFOR
   cgPS_Close,/png,Density=300,Resize=cal_plot_resize,/allow_transparent,/nomessage
ENDIF

END    
