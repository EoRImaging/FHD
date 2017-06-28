#### Submit pipe_slurm job

nohup ./pipe_slurm.sh -f FilteredMWA -m 30G -w 03:30:00 -v 'mwa_filtered' > nohup.out &

#nohup ./pipe_slurm.sh -f GoldenSubset -m 30G -w 02:30:00 -t 1 -v 'mwa_goldensubset' > nohup.out &
