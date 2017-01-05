
# Simulate MWA128 with PAPER beams
#nohup ./sim_pipe_slurm.sh -f Hera128 -v 'sim_paper128_at_MRO' -m 30G -w 06:00:00 > nohup_paper128mro.out &

# Simulate PAPER-128   (-t = firstpass_only)
#nohup ./sim_pipe_slurm.sh -f PAPER128 -t 1 -v 'sim_paper128-b' -m 30G -w 06:00:00 > nohup_paper128.out &
#nohup ./sim_pipe_slurm.sh -f PAPER128 -t 1 -v 'sim_paper128' -m 30G -w 06:00:00 > nohup_paper128.out &
#nohup ./sim_pipe_slurm.sh -f PAPER128 -t 1 -v 'sim_paper128_1hour' -m 30G -w 06:00:00 > nohup_paper128.out &
#nohup ./sim_pipe_slurm.sh -f EWBASE -s 'ewbase_paper-a_0'  -e "ewbase_paper-a_4" -t 1 -v 'sim_ewbase-a' -m 30G -w 06:00:00 > nohup_ewbase-a.out &   # Phased to first time step of each.
#nohup ./sim_pipe_slurm.sh -f EWBASE -t 1 -v 'sim_ewbase-b' -m 30G -w 06:00:00 > nohup_ewbase-b.out &   # All phased to the same start
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-c_0'  -e "ewbase_paper-c_4" -t 1 -v 'sim_ewbase-c' -m 30G -w 06:00:00 > nohup_ewbase-c.out &   # Phased to first time step of first
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-d_0'  -e "ewbase_paper-d_4" -t 1 -v 'sim_ewbase-d' -m 30G -w 06:00:00 > nohup_ewbase-d.out &   # Phased by pyuvdata
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-f_0'  -e "ewbase_paper-f_4" -t 1 -v 'sim_ewbase-f' -m 30G -w 06:00:00 > nohup_ewbase-f.out &   # Phased by pyuvdata, epoch set to current jultime, phased to time instead of ra/dec
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-g_0'  -e "ewbase_paper-g_4" -t 1 -v 'sim_ewbase' -m 30G -w 06:00:00 > nohup_ewbase-g.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-h_0'  -e "ewbase_paper-h_4" -t 1 -v 'sim_ewbase' -m 30G -w 06:00:00 > nohup_ewbase-h.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially, and shifted by 43 sec
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-i_0'  -e "ewbase_paper-i_4" -t 1 -v 'sim_ewbase' -m 30G -w 06:00:00 > nohup_ewbase-i.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-i_0'  -e "ewbase_paper-i_4" -t 1 -v 'sim_ewbase_onesrc' -m 30G -w 06:00:00 > nohup_ewbase-i.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-j_0'  -e "ewbase_paper-j_4" -t 1 -v 'sim_ewbase_onesrc' -m 30G -w 06:00:00 > nohup_ewbase-i.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially, phased to same point
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-j_0'  -e "ewbase_paper-j_4" -t 1 -v 'sim_ewbase-mwa_onesrc' -m 30G -w 06:00:00 > nohup_ewbase-i.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially, phased to same point, with MWA beam
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-k_0'  -e "ewbase_paper-k_4" -t 1 -v 'sim_ewbase_onesrc' -m 30G -w 06:00:00 > nohup_ewbase-k.out &   # only one source, and a range of frequencies set to 4 (no phase) artificially, phased to start of each file by time, paper beams
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-k_0'  -e "ewbase_paper-k_4" -t 1 -v 'sim_ewbase_onesrc3' -m 30G -w 06:00:00 > nohup_ewbase-k.out &   # only one source, moved to RA=0
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-l_28'  -e "ewbase_paper-l_32" -t 1 -v 'sim_ewbase_onesrc3' -m 30G -w 06:00:00 > nohup_ewbase-k.out &   # 2 sec integration time
#nohup ./sim_pipe_slurm.sh -f EWBASE  -s 'ewbase_paper-k_0'  -e "ewbase_paper-k_4" -t 1 -v 'sim_ewbase_100src' -m 30G -w 06:00:00 > nohup_ewbase-k.out &  #100 sources, nonzero beam_offset_time
#nohup ./sim_pipe_slurm.sh -f EWBASE-n -s 'ewbase_paper-n_157' -t 1 -v 'sim_ewbase_100src' -m 30G -w 01:00:00 > nohup_ewbase-n.out &  #one 2sec integrations per file. one baseline, 10 min total
nohup ./sim_pipe_slurm.sh -f EWBASE_MWA -m 30G -w 01:30:00 -t 1 -v 'sim_ewbase_onesrc' > nohup_ewbase_mwa.out &   #MWA beams, delays in place, zenith pointing, file separation carefully set.

#nohup ./sim_pipe_slurm.sh -f MWAGolden  -e '1061312272' -t 1 -v 'sim_mwa_golden' -m 30G -w 06:00:00 > nohup_mwa128.out &   # A simulation of MWA128 golden set files
