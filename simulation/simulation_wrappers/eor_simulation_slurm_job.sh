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
# #SBATCH -p skylake

# obs_id, outdir, version, and thresh and ncores expected to be passed from sbatch call
# in sim_pipe_slurm.sh

echo JOBID ${SLURM_ARRAY_JOB_ID}
echo TASKID ${SLURM_ARRAY_TASK_ID}

#ls $outdir > /dev/null # ping the output directory so nfs automounts

module load ghostscript
module load imagemagick/7.0.7
module load git/2.10.2
module load idl
shopt -s expand_aliases; source $IDL/envi53/bin/envi_setup.bash

obsids=("$@")
obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}

#ls $outdir > /dev/null # ping the output directory so nfs automounts
idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $ncores -e sim_versions_wrapper -args $obs_id $outdir $version $sim_id

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

