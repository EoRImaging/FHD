PRO mwa_tile_locate,data_directory=data_directory,filename=filename,obs=obs,params=params,psf=psf

vis_path_default,data_directory,filename,file_path,obs=obs
output_directory=data_directory+'\Images\Tiles'
file_id='_xx00_00'

IF N_Elements(pol_i) EQ 0 THEN pol_i=0
pol_name=(['xx','yy','xy','yx'])[pol_i]
IF N_Elements(obs) EQ 0 THEN restore,filename=file_path+'_obs.sav' ;obs struct
IF N_Elements(params) EQ 0 THEN restore,filename=file_path+'_params.sav' ;params struct
IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,/restore_last) ;struct

;extract information from the structures
dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span

freq_bin_i=obs.fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=obs.bin_offset
frequency_array=obs.freq
tile_A=obs.tile_A
tile_B=obs.tile_B
n_tiles=Max(tile_A)-Min(tile_A)+1 ;tile indices start from 1 not 0!

psf_base=psf.base
psf_dim=(Size(*psf_base[0],/dimension))[0]
psf_resolution=(Size(psf_base,/dimension))[2]

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)

tile_img_arr=Ptrarr(n_tiles,/allocate)
FOR tile_i=0,n_tiles-1 DO *tile_img_arr[tile_i]=fltarr(dimension,elements)

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(Temporary(xcen)+x_offset/psf_resolution+dimension/2.)-psf_dim/2.) 
ymin=Floor(Round(Temporary(ycen)+y_offset/psf_resolution+elements/2.)-psf_dim/2.) 

xmin[*,where(kx_arr EQ 0)]=-1
ymin[*,where(kx_arr EQ 0)]=-1
FOR ti=0,n_tiles-1 DO BEGIN
    cont=0
    i_useA=where(tile_A EQ (ti+1),nA)
    i_useB=where(tile_B EQ (ti+1),nB)
    CASE 1 OF 
        (nA GT 0) AND (nB GT 0):i_use=[i_useA,i_useB]
        nA GT 0:i_use=i_useA
        nB GT 0:i_use=i_useB
        ELSE: cont=1
    ENDCASE
    IF Keyword_Set(cont) THEN CONTINUE
    xmin_use=xmin[*,i_use]
    ymin_use=ymin[*,i_use]
    
    bin_n=histogram(xmin_use+ymin_use*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
    bin_i=where(bin_n,n_bin_use);+bin_min
    
    tile_img=fltarr(dimension,elements)
    FOR bi=0,n_bin_use-1 DO BEGIN
        ind=ri[ri[bin_i[bi]]]
        xmin1=xmin_use[ind] & xmax1=xmin1+psf_dim-1
        ymin1=ymin_use[ind] & ymax1=ymin1+psf_dim-1
        tile_img[xmin1:xmax1,ymin1:ymax1]+=1.
    ENDFOR
    tile_img+=Shift(Reverse(reverse(tile_img,1),2),1,1)
    *tile_img_arr[ti]=tile_img
ENDFOR

tile_img_arr_out=Ptrarr(n_tiles,/allocate)

IF file_test(rootdir('mwa')+output_directory) EQ 0 THEN file_mkdir,rootdir('mwa')+output_directory

tile_names=("Tile_"+Sindgen2(n_tiles+1))
FOR tile_i=0,n_tiles-1 DO BEGIN
    *tile_img_arr_out[tile_i]=fltarr(dimension,elements)
    i_use=where(*tile_img_arr[tile_i],n_use)
    IF n_use GT 0 THEN (*tile_img_arr_out[tile_i])[i_use]=tile_i+1. ELSE CONTINUE
    Imagefast,*tile_img_arr_out[tile_i],filename=tile_names[tile_i+1]+" UV distribution",title=tile_names[tile_i+1],$
        data_dir=output_directory,/no_colorbar,low=0,high=n_tiles,color_table=33,background='white',/no_ps,/zero_white
    FitsFast,*tile_img_arr_out[tile_i],filename=tile_names[tile_i+1]+" UV distribution",/write,root=rootdir('mwa'),$
        filepathfull=output_directory
ENDFOR
END