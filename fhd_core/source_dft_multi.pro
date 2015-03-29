PRO source_dft_multi,obs,jones,source_array,model_uv_full,spectral_uv_full,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,$
    conserve_memory=conserve_memory,frequency=frequency,dft_threshold=dft_threshold,$
    dimension=dimension,elements=elements,n_pol=n_pol,spectral_taylor_expand=spectral_taylor_expand,_Extra=extra

IF Keyword_Set(obs) THEN BEGIN
    IF N_Elements(dft_threshold) EQ 0 THEN dft_threshold=obs.dft_threshold
    dimension=obs.dimension
    elements=obs.elements
    n_pol=obs.n_pol
ENDIF ELSE BEGIN
    IF N_Elements(dft_threshold) EQ 0 THEN dft_threshold=0.
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    IF N_Elements(n_pol) EQ 0 THEN n_pol=1
ENDELSE

IF N_Elements(uv_i_use) EQ 0 THEN uv_i_use=Lindgen(dimension*elements)

IF N_Elements(xvals) NE N_Elements(uv_i_use) THEN xvals=(meshgrid(dimension,elements,1))[uv_i_use]-dimension/2
IF N_Elements(yvals) NE N_Elements(uv_i_use) THEN yvals=(meshgrid(dimension,elements,2))[uv_i_use]-elements/2

x_vec=source_array.x
y_vec=source_array.y

;set /no_extend since extended sources will not be read. 
; If you want extended sources, inflate the source list before calling this program
source_array_use=Stokes_cnv(source_array,jones,/inverse,/no_extend,_Extra=extra) 

frequency=obs.freq_center
;IF Keyword_Set(frequency) THEN BEGIN
    freq_ref=Median(source_array.freq)
    freq_ratio=Abs(Alog10(freq_ref/frequency)) ;it often happens that one is in Hz and the other in MHz. Assuming no one will ever want to extrapolate more than two orders of magnitude, correct any huge mismatch
    IF freq_ratio GT 2 THEN freq_scale=10.^(Round(Alog10(freq_ref/frequency)/3.)*3.) ELSE freq_scale=1.
    frequency_use=frequency*freq_scale
    
    alpha_i=where(source_array.alpha,n_alpha) ;find sources with non-zero spectral indices
    FOR a_i=0L,n_alpha-1 DO BEGIN
        flux_scale=(frequency_use/freq_ref)^source_array[alpha_i[a_i]].alpha
        FOR pol_i=0,n_pol-1 DO source_array_use.flux.(pol_i)*=flux_scale
    ENDFOR
;ENDIF

IF Tag_exist(obs,'degrid_info') THEN IF Ptr_valid(obs.degrid_info) THEN BEGIN
    freq_arr=(*obs.degrid_info).freq
;    freq_bin_i=(*obs.degrid_info).bin_i
    nfreq_bin=N_Elements(freq_arr)
    
    IF N_Elements(spectral_taylor_expand) EQ 0 THEN spectral_taylor_expand=1
    
    IF spectral_taylor_expand GE 1 THEN BEGIN
        alpha_arr=source_array.alpha
        
        flux_arr=Ptrarr(n_pol,spectral_taylor_expand+1)
        FOR pol_i=0,n_pol-1 DO BEGIN
            flux_arr[pol_i,0]=Ptr_new(source_array_use.flux.(pol_i))
            FOR s_i=1.,spectral_taylor_expand DO flux_arr[pol_i,s_i]=Ptr_new(source_array_use.flux.(pol_i)*alpha_arr^s_i)
        ENDFOR
        
        IF Keyword_Set(dft_threshold) THEN BEGIN
            IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=0
            model_image_arr=fast_dft(x_vec,y_vec,dimension=dimension,elements=elements,flux_arr=flux_arr,return_kernel=return_kernel,$
                conserve_memory=conserve_memory,dft_threshold=dft_threshold,/no_fft)
            
            Ptr_free,flux_arr
        ENDIF ELSE BEGIN
            IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
            model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,flux=flux_arr,conserve_memory=conserve_memory)
            model_image_arr=Ptrarr(n_pol,spectral_taylor_expand+1)
            FOR pol_i=0,n_pol-1 DO BEGIN
                FOR s_i=0,spectral_taylor_expand DO BEGIN ;no "-1" for second loop!
                    single_uv=Complexarr(dimension,elements)
                    single_uv[uv_i_use]=*model_uv_vals[i]
                    single_img=fft_shift(FFT(fft_shift(Temporary(single_uv)))) 
                    model_image_arr[pol_i,s_i]=Ptr_new(single_img,/no_copy)
                ENDFOR
            ENDFOR
            
            Ptr_free,model_uv_vals,flux_arr
        ENDELSE
        spectral_index_arr=model_image_arr[*,1:*]
        image_ref=model_image_arr[*,0]
        
        model_uv_cube=Ptrarr(n_pol,nfreq_bin)
        FOR pol_i=0,n_pol-1 DO model_uv_cube[pol_i,*]=$
            source_dft_cube(*image_ref[pol_i],spectral_index_powers_arr=spectral_index_arr[pol_i,*],freq_arr=freq_arr,freq_ref=frequency)
        
    ENDIF ELSE BEGIN ;else: do not use a taylor series expansion, and calculate the dft at every frequency (computationally expensive!)        
        flux_arr=Ptrarr(n_pol,nfreq_bin)
        FOR pol_i=0,n_pol-1 DO BEGIN
            FOR freq_i=0,nfreq_bin-1 DO BEGIN
                flux_arr[pol_i,freq_i]=Ptr_new(source_array_use.flux.(pol_i)*(freq_arr[freq_i]/frequency)^alpha_arr)
            ENDFOR
        ENDFOR
        IF Keyword_Set(dft_threshold) THEN BEGIN
            IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=0
            model_uv_cube=fast_dft(x_vec,y_vec,dimension=dimension,elements=elements,flux_arr=flux_arr,return_kernel=return_kernel,$
                conserve_memory=conserve_memory,dft_threshold=dft_threshold)
            Ptr_free,flux_arr
        ENDIF ELSE BEGIN
            IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
            model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,flux=flux_arr,conserve_memory=conserve_memory)
            model_uv_cube=Ptrarr(n_pol,nfreq_bin)
            FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO BEGIN
                model_uv_cube[pol_i,freq_i]=Ptr_new(complexarr(dimension,elements))
                (*model_uv_cube[pol_i,freq_i])[uv_i_use]=*model_uv_vals[pol_i,freq_i]
            ENDFOR
            Ptr_free,model_uv_vals,flux_arr
        ENDELSE
    ENDELSE
    
    IF Max(Ptr_valid(model_uv_full)) GT 0 THEN BEGIN
        ;still need some sort of test in case model_uv_full is already a cube!
        FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO *model_uv_cube[pol_i,freq_i]+=*model_uv_full[pol_i]
        Ptr_free,model_uv_full
    ENDIF
    model_uv_full=mode_uv_cube 
ENDIF ELSE BEGIN
;in this case, grid one continuum image for each polarization (no frequency dimension)
    flux_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO flux_arr[pol_i]=Ptr_new(source_array_use.flux.(pol_i))
    IF Max(Ptr_valid(model_uv_full)) EQ 0 THEN BEGIN
        model_uv_full=Ptrarr(n_pol,/allocate)
        FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]=Complexarr(dimension,elements)
    ENDIF
    IF Keyword_Set(dft_threshold) THEN BEGIN
        IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=0
        model_uv_new=fast_dft(x_vec,y_vec,dimension=dimension,elements=elements,flux_arr=flux_arr,return_kernel=return_kernel,$
            conserve_memory=conserve_memory,dft_threshold=dft_threshold)
        FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]+=*model_uv_new[pol_i]
        Ptr_free,model_uv_new,flux_arr
    ENDIF ELSE BEGIN
        IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
        model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,flux=flux_arr,conserve_memory=conserve_memory)
        FOR pol_i=0,n_pol-1 DO (*model_uv_full[pol_i])[uv_i_use]+=*model_uv_vals[pol_i]
        Ptr_free,model_uv_vals,flux_arr
    ENDELSE
ENDELSE
END