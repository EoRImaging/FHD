
#obsid_list_files/HeraPlatinumFakeShort


nohup ./sim_pipe_slurm.sh -t -e 'hera19_platinum_short_1' -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 03:30:00 -v 'sim_hera19_point_doubles'  > nohup_hera_point.out &

#nohup ./sim_pipe_slurm.sh -t  -f obsid_list_files/SemiCirc_comp_1hour -n 5 -m 30G  -w 02:30:00  -v 'sim_semicircle_point' > nohup_semicircle.out &

#nohup ./sim_pipe_slurm.sh -t -f obsid_list_files/SemiCirc_comp_1hour -n 5 -m 30G  -w 02:30:00  -v 'sim_semicircle_point_interp' > nohup_semicircle.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_0' -n 40 -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 1:30:00  -v 'sim_paper19_eor_comp_more' > nohup_eor.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_0' -n 40 -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 1:30:00  -v 'sim_hera19_eor_comp_more' > nohup_eor.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_0' -e 'hera19_platinum_short_33' -n 40 -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 1:30:00  -v 'sim_hera19_eflat_comp' > nohup_flat.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_0' -n 40 -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 1:30:00  -v 'sim_hera19_eflat_comp_more' > nohup_flat_more.out &

#nohup ./sim_variation.sh -t -f ewbase_14m_plat -n 20 -m 80G -w 03:30:00 -v 'sim_ewbase_hera_beam-mask-thresh' -p 'beam_mask_threshold=50.0,75.0,100.0,125.0,150.0' > nohup_14m_ewbase.out &

#nohup ./sim_pipe_slurm.sh -t -e 'hera19_platinum_short_2' -f obsid_list_files/HeraPlatinumFakeShort -m 50G -w 03:30:00 -v 'sim_hera19_point_interp-kern'  > nohup_hera_point.out &

#nohup ./sim_pipe_slurm.sh -t  -f redo -n 5 -m 30G  -w 02:30:00  -v 'sim_semicircle_point' > nohup_semicircle.out &

#nohup ./sim_variation.sh -t -f ewbase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_kbin_t2_v2' -p 'kbinsize=0.050' > nohup_14m_ewbase_2.out &
#nohup ./sim_variation.sh -t -f ewbase_14m_plat -m 180G -w 03:30:00 -v 'sim_ewbase_hera_kbin_t2_v2' -p 'kbinsize=0.050,0.054,0.057' > nohup_14m_ewbase_2.out &

#nohup ./sim_variation.sh -t -f ewbase_14m_plat -m 70G -w 03:30:00 -v 'sim_ewbase_mwa_kbin_few-src' -p 'kbinsize=0.075,0.078,0.081,0.08,40.086,0.088' > nohup_14m_ewbase_1.out &

#nohup ./sim_variation.sh -t -f ewbase_14m_plat -m 70G -w 03:30:00 -v 'sim_ewbase_hera_kbin_t2_v2' -p 'kbinsize=0.075,0.078,0.081,0.084' > nohup_14m_ewbase_1.out &

#nohup ./sim_variation.sh -t -f ewbase_14m_plat -m 90G -w 03:30:00 -v 'sim_ewbase_mwa_kbin_few-src' -p 'kbinsize=0.063,0.066,0.069,0.072' > nohup_14m_ewbase_2.out &

#nohup ./sim_pipe_slurm.sh -t  -s 'hera37_platinum_0' -e 'hera37_platinum_19' -f obsid_list_files/Hera37PlatinumFake -n 50 -m 50G  -w 02:30:00  -v 'sim_hera37_eflat' > nohup_hera37.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_0' -e 'hera19_platinum_short_33' -n 50 -f obsid_list_files/HeraPlatinumFakeShort -m 40G -w 1:30:00  -v 'sim_paper19_eflat_comp' > nohup_eor.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera19_platinum_short_1' -e 'hera19_platinum_short_33' -f obsid_list_files/HeraPlatinumFakeShort -m 50G -w 03:30:00 -v 'sim_hera19_diffuse_kelvin-convert'  > nohup_hera_diffuse.out &

#nohup ./sim_pipe_slurm.sh -t -e 'hera19_platinum_short_33' -f obsid_list_files/HeraPlatinumFakeShort -m 50G -w 03:30:00 -v 'sim_hera19_point_fine_comp_1hour'  > nohup_hera_point.out &

#nohup ./sim_pipe_slurm.sh -t -e 'mwa_galaxy_compressed_1' -f  obsid_list_files/MWA128GalaxyTransit -m 50G -w 03:30:00 -v 'sim_mwa_diffuse_new-FHD'  > nohup_mwa_diffuse.out &

#nohup ./sim_pipe_slurm.sh -t -f obsid_list_files/EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_diffuse'  > nohup_14m_ewbase_0.out &


#nohup ./sim_variation.sh -t -f ewbase_28m_plat_0 -m 60G -w 04:30:00 -v 'sim_ewbase_hera_nfreqavg' -p 'nfreq_avg=203,101,50,25' > nohup_28m_ewbase_2.out &


#nohup ./sim_variation.sh -t -f ewbase_28m_plat_0 -m 50G -w 03:30:00 -v 'sim_ewbase_hera_kbin_v2' -p 'kbinsize=0.1,0.2,0.5' > nohup_28m_ewbase_1.out &
#nohup ./sim_variation.sh -t -f ewbase_28m_plat_0 -m 150G -w 03:30:00 -v 'sim_ewbase_hera_kbin_v2' -p 'kbinsize=0.05,0.06,0.07,0.08' > nohup_28m_ewbase_2.out &


#nohup ./sim_pipe_slurm.sh -f EoR1_selected  -m 50G -w 6:30:00  -v 'sim_mwa_fornax_mwabeams' > nohup_mwa128.out &

#nohup ./sim_pipe_slurm.sh -f EoR1_selected  -m 50G -w 6:30:00  -v 'sim_mwa_fornax_paperbeams' > nohup_paper-mwa128.out &

#nohup ./sim_pipe_slurm.sh -f EoR1_selected  -m 50G -w 6:30:00  -v 'sim_mwa_fornax_herabeams' > nohup_hera-mwa128.out &

#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_psf-20' > nohup_hera20.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_psf-50' > nohup_hera50.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_psf-80' > nohup_hera80.out &

#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_psf-200' > nohup_hera200.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_psf-150' > nohup_hera150.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_psf-120' > nohup_hera120.out &

## Run a single 14m baselines, E-W, with 7000 sources at different kspace resolutions with HERA beams.
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_0.08' > nohup_hera08.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_0.07' > nohup_hera07.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_0.06' > nohup_hera06.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_0.05' > nohup_hera05.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_0.1' > nohup_hera1.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_0.2' > nohup_hera2.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_0.5' > nohup_hera5.out &


## Run a single 14m baselines, E-W, with 7000 sources at different kspace resolutions with HERA beams.  Using v2 HERA beam
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.08' > nohup_hera08.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.07' > nohup_hera07.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.06' > nohup_hera06.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.05' > nohup_hera05.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.1' > nohup_hera1.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.09' > nohup_hera09.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.15' > nohup_hera15.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.2' > nohup_hera2.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_hera_v2_0.5' > nohup_hera5.out &

### Run a single 14m baselines, E-W, with 7000 sources at different kspace resolutions with paper beams.
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_paper_0.08' > nohup_paper08.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_paper_0.07' > nohup_paper07.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_paper_0.06' > nohup_paper06.out &
#nohup ./sim_pipe_slurm.sh -t -f EWBase_14m_plat -m 150G -w 03:30:00 -v 'sim_ewbase_paper_0.05' > nohup_paper05.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 50G -w 03:30:00 -v 'sim_ewbase_paper_0.1' > nohup_paper1.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_paper_0.2' > nohup_paper2.out &
#nohup ./sim_pipe_slurm.sh -f EWBase_14m_plat -m 30G -w 03:30:00 -v 'sim_ewbase_paper_0.5' > nohup_paper5.out &




### Simulate EoR with MWA-128, full gleam catalog, but no metafits file.

#if [ -f '/gpfs/data/jpober/alanman/EoR1/1066675616.metafits' ]; then
#	echo "Metafits exists"
#	mv ~/data/alanman/EoR1/1066675616.metafits ~/data/alanman/EoR1/.1066675616.metafits
#fi
#nohup ./sim_pipe_slurm.sh -f EoR1_selected -s 1066675616 -e 1066675616  -m 90G -w 30:30:00  -v 'sim_mwa_fornax_eor_nometa' > nohup_mwa128_nometa.out &


#if [ ! -f '/gpfs/data/jpober/alanman/EoR1/1066675616.metafits' ]; then
#	echo "Moving metafits back"
#	mv ~/data/alanman/EoR1/.1066675616.metafits ~/data/alanman/EoR1/1066675616.metafits
#fi
#nohup ./sim_pipe_slurm.sh -f EoR1_selected -s 1066675616 -e 1066675616  -m 90G -w 30:30:00  -v 'sim_mwa_fornax_eor_meta' > nohup_mwa128.out &
#nohup ./sim_pipe_slurm.sh -f EoR1_selected_fake -m 90G -w 30:30:00  -v 'sim_mwa_fornax_eor_meta' > nohup_mwa128.out &
#nohup ./sim_pipe_slurm.sh -f EoR1_selected_fake -m 90G -w 30:30:00  -v 'sim_mwa_fornax_eor_paperbeam' > nohup_paper-mro.out &

#nohup ./sim_pipe_slurm.sh -f EoR1_selected_fake -m 90G -w 48:30:00  -v 'sim_mwa_fornax_eor_herabeam' > nohup_hera128.out &

#nohup ./sim_pipe_slurm.sh -s 'hera_platinum_0' -e 'hera_platinum_0' -f HeraPlatinumFake -m 30G -w 7:30:00  -v 'sim_hera19_lidz_eor' > nohup_hera19_eor.out &

#nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_paper19' > nohup_paper19.out &
#nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_paper19_lowres' > nohup_paper19.out &
#nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_hera19' > nohup_hera19.out &
#nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f obsid_list_files/HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_hera19_v2' > nohup_hera19.out &
#nohup ./sim_pipe_slurm.sh -t -f obsid_list_files/HeraPlatinumFakeShort -m 20G -w 03:30:00  -v 'sim_hera19_v2' > nohup_hera19.out &
#nohup ./sim_pipe_slurm.sh -t -f obsid_list_files/HeraPlatinumFakeShort -m 50G -w 03:30:00  -v 'sim_hera19_v2_sidelobes' > nohup_hera19.out &

#nohup ./sim_pipe_slurm.sh -t  -s 'hera37_platinum_0' -e 'hera37_platinum_19' -f Hera37PlatinumFake -m 30G  -w 03:30:00  -v 'sim_hera37' > nohup_hera37.out &

#nohup ./sim_pipe_slurm.sh -t  -s 'paper128_platinum_0' -e 'paper128_platinum_19' -f Paper128PlatinumFake -m 60G  -w 03:30:00  -v 'sim_paper128' > nohup_paperplat.out &

#nohup ./sim_pipe_slurm.sh -t  -s 'hera_platinum_16' -e 'hera_platinum_16' -f HeraPlatinumFake -m 60G  -w 02:30:00  -v 'sim_hera19_hires' > nohup_hera19.out &
