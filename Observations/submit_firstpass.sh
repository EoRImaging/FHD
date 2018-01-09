#### Submit pipe_slurm job

#nohup ./pipe_slurm.sh -f FilteredMWA -m 30G -w 03:30:00 -v 'mwa_filtered' > nohup.out &

nohup ./pipe_slurm.sh -f GoldenSubset -m 60G -w 04:30:00 -n 16 -v 'mwa_goldensubset_avg' > nohup.out &
