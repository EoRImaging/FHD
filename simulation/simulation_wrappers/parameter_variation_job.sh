#! /bin/bash
#############################################################################
#Runs one observation at a time in slurm.  Second level program for 
#running simulations on Oscar. First level program is 
#sim_pipe_slurm.sh
#############################################################################

#SBATCH -J array_sim
# Email if the job fails
#SBATCH --mail-type=FAIL     
#SBATCH --mail-user=adam_lanman@brown.edu

# obs_id, outdir, version, and thresh and ncores expected to be passed from sbatch call
# in sim_pipe_slurm.sh

echo JOBID ${SLURM_ARRAY_JOB_ID}
echo TASKID ${SLURM_ARRAY_TASK_ID}

#ls $outdir > /dev/null # ping the output directory so nfs automounts

module load ghostscript
module load imagemagick/6.6.4
module load git/2.2.1

obs_id=$1
#obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}

## Parse the param_variation into an array and select based on task_id
echo "Param_var = "$param_var
variable=$(python -c "print \"$param_var\".split('=')[0]")
values=$(python -c "print \"$param_var\".split('=')[1]")
value=$(python -c "print \"$values\".split('_')[${SLURM_ARRAY_TASK_ID}]")

#ls $outdir > /dev/null # ping the output directory so nfs automounts
/usr/local/bin/idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $ncores -e sim_versions_wrapper -args $obs_id $outdir $version $variable"="$value 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

