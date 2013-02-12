PRO batch_eor,obsidfile,version=version
textfast,obsids,file_path=obsidfile,/string,/read
datadirectory=file_dirname(obsidfile)
files = datadirectory+obsids
healpix_path=datadirectory+'/fhd_healpix_v'+strcompress(string(version),/removeall)
general_obs,data_directory=files,version=version,healpix_path=healpix_path
