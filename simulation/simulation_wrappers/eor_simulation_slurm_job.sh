#! /bin/bash
#############################################################################
#Runs one observation at a time in slurm.  Second level program for 
#running simulations on Oscar. First level program is 
#sim_pipe_slurm.sh
#############################################################################

#SBATCH -J array_sim
# #SBATCH --mail-type=ALL
# #SBATCH --mail-user=adam_lanman@brown.edu

# obs_id, outdir, version, and thresh and ncores expected to be passed from sbatch call
# in sim_pipe_slurm.sh

echo JOBID ${SLURM_ARRAY_JOB_ID}
echo TASKID ${SLURM_ARRAY_TASK_ID}

#ls $outdir > /dev/null # ping the output directory so nfs automounts

module load ghostscript
module load imagemagick/6.6.4
module load git/2.2.1

obsids=("$@")
obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}

#ls $outdir > /dev/null # ping the output directory so nfs automounts
/usr/local/bin/idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $ncores -e sim_versions_wrapper -args $obs_id $outdir $version 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

