PRO batch_eor,obsidfile,version=version,_Extra=extra

textfast,obsids,file_path=obsidfile,/string,/read
datadirectory=file_dirname(obsidfile)+'/'
files = datadirectory+obsids
healpix_path=datadirectory+'/fhd_healpix_v'+strcompress(string(version),/remove_all)
general_obs,data_directory=files,version=version,healpix_path=healpix_path,_Extra=extra

END