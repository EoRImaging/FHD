#! /bin/bash
#############################################################################
#Slurm jobscript for running a single ObsID.  Second level program for 
#running firstpass on Oscar. First level program is 
#batch_firstpass.sh
#############################################################################

#SBATCH -J firstpass
# #SBATCH --mail-type=ALL
# #SBATCH --mail-user=adam_lanman@brown.edu


module load ghostscript
module load imagemagick/6.6.4
module load git/2.2.1

obsids=("$@")
obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}

/usr/local/bin/idl -IDL_DEVICE ps -quiet -IDL_CPU_TPOOL_NTHREADS $ncores -e firstpass_versions_wrapper -args $obs_id $outdir $version 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

