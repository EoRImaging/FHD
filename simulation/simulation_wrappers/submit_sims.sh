##### Simulate PAPER-128   (-t = firstpass_only)

##10,000 sources, 9 hours, data only, no_extend=1
#nohup ./sim_pipe_slurm.sh -f PAPER128 -m 30G -w 02:30:00 -t 1 -v 'sim_paper128_9hour_noextend' > nohup_paper128_9hour.out &

# EoR simulation test
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s "PAPER128_22" -e "PAPER128_23" -v 'sim_eor_paper128' -m 50G -w 02:00:00 > nohup_paper128_eor.out &
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s "PAPER128_22" -e "PAPER128_22" -v 'sim_eor_paper128_2' -m 90G -w 04:00:00 > nohup_paper128_eor.out &


# EoR simulation -- MWA128 with PAPER beams
#nohup ./sim_pipe_slurm.sh -f MWAGolden -s "1061311664" -e "1061311664" -v 'sim_eor_paper128_at_MRO' -m 90G -w 04:00:00 > nohup_paper128_eor.out &

##fornax only, near zenith, four files.
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s 'PAPER128_22' -e "PAPER128_53"  -t 1 -v 'sim_paper128_onlyfornax2' -m 30G -w 02:00:00 > nohup_paper128_1src.out &


## 10,000 sources, 9 hours, no fornax

#nohup ./sim_pipe_slurm.sh -f PAPER128 -m 30G -w 02:30:00 -t 1 -v 'sim_paper128_9hour_nofornax' > nohup_paper128_nofor.out &


## 10,000 sources, 9 hours, fornax, but only one beam model.
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s 'PAPER128_26' -e 'PAPER128_28' -m 30G -w 02:30:00 -t 1 -v 'sim_paper128_9hour_1coarse' > nohup_paper128_nofor.out &

##One source, near zenith, four files.
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s 'PAPER128_22' -e "PAPER128_26"  -t 1 -v 'sim_paper128_1src' -m 30G -w 02:00:00 > nohup_paper128_1src.out &

##10,000 sources, four files, 
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s 'PAPER128_22' -e "PAPER128_26"  -t 1 -v 'sim_paper128_10000src_diffuse' -m 30G -w 02:00:00 > nohup_paper128_diffuse.out &

##10,000 sources, 9 hours, data only.
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s "PAPER128_1" -e 'PAPER128_1'  -m 80G -w 04:30:00  -v 'sim_eor_paper128_9hour' > nohup_paper128_9hour.out &

## 10 sources, simple.
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s 'PAPER128_0' -e 'PAPER128_1'  -m 10G -w 00:30:00 -t 1 -v 'test' > nohup_test.out &

#nohup ./sim_pipe_slurm.sh -f PAPER128 -s "PAPER128_1" -e 'PAPER128_1'  -m 80G -w 04:30:00  -v 'sim_paper128_louvres' > nohup_paper128_hires.out &
#nohup ./sim_pipe_slurm.sh -f PAPER128 -s "PAPER128_1" -e 'PAPER128_1'  -m 50G -w 04:30:00  -v 'sim_paper128_louvres' > nohup_paper128_hires.out &


##### Simulate MWA-128


#nohup ./sim_pipe_slurm.sh -f MWAGolden -s '1061311664' -e '1061312520' -m 60G -w 03:30:00  -v 'sim_mwa_golden' > nohup_mwa128.out &
#nohup ./sim_pipe_slurm.sh -f MWAGolden -s '1061311664' -e '1061312520' -m 65G -w 03:30:00  -v 'sim_mwa_golden2' > nohup_mwa128.out &


##### Simulate HERA-37


#nohup ./sim_pipe_slurm.sh -f HERA37 -m 30G -w 02:30:00  -v 'sim_hera37_3hour' > nohup_hera37.out &


#### Simulate HERA-19

#nohup ./sim_pipe_slurm.sh -f HERA19 -m 30G -s 'HERA19_22' -e 'HERA19_26' -w 02:30:00  -v 'sim_hera19_9hour' > nohup_hera19.out &
#nohup ./sim_pipe_slurm.sh -f HERA19 -m 30G  -w 03:30:00  -v 'sim_hera19_9hour' > nohup_hera19.out &

#EoR tests
nohup ./sim_pipe_slurm.sh -f HERA19 -s "HERA19_22" -e "HERA19_26" -v 'sim_eor_hera19_mwabeam_test' -m 50G -w 02:00:00 > nohup_hera19_eor.out &
#nohup ./sim_pipe_slurm.sh -f HERA19 -s "HERA19_22" -e "HERA19_22" -v 'sim_eor_hera19_test' -m 50G -w 02:00:00 > nohup_hera19_eor_t.out &

#nohup ./sim_pipe_slurm.sh -f HERA19 -m 120G  -w 03:30:00  -v 'sim_hera19_9hour_hiUVres' > nohup_hera19.out &

#EW Base phase_to_time() vs phase() test
#nohup ./sim_pipe_slurm.sh -f PHASETEST -m 40G  -w 02:30:00  -v 'sim_ewbase_phasetest' > nohup_ewbase.out &


#### Simulate HERA platinum set
#nohup ./sim_pipe_slurm.sh -f HeraPlatinum -s 'zen.2457458.17389.xx.HH.uvcU' -e 'zen.2457458.19477.xx.HH.uvcU' -v 'sim_heraplat_30min' -m 60G -w 02:00:00 > nohup_heraplat.out &

#nohup ./sim_pipe_slurm.sh -f HeraPlatinumFake -s 'hera_platinum_fine_0' -e 'hera_platinum_fine_15' -v 'sim_heraplat_30min_fake_morebeams' -m 60G -w 02:00:00 > nohup_heraplat.out &

