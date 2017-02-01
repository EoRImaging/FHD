
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

nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_paper19' > nohup_paper19.out &
nohup ./sim_pipe_slurm.sh -t -s 'hera_platinum_0' -e 'hera_platinum_19' -f HeraPlatinumFake -m 30G  -w 03:30:00  -v 'sim_hera19' > nohup_hera19.out &
