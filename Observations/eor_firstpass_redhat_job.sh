#! /bin/bash
#############################################################################
#Runs one observation at a time on Slurm.  Second level program for 
#running firstpass on the MIT machines. First level program is 
#batch_firstpass.sh
#############################################################################

#SBATCH -J firstpass
# #SBATCH --mail-type=ALL
# #SBATCH --mail-user=adam_lanman@brown.edu

# obs_id, outdir, version, and thresh and nslots expected to be passed from qsub call
# in batch_firstpass.sh

## Print all environmental variables
#printenv

#ls $outdir > /dev/null # ping the output directory so nfs automounts

## TODO: check wedge stats and stop if above thresh
#if [ "${thresh}" -gt "0" ]; then
#    # check db for wedgie
#    echo Checking wedgie
#    export PGPASSWORD=BowTie	
#    # first check that there is an entry
#    exists=`psql -h eor-00 mwa_qc -U mwa -c "select count(*) from qs where obsid=${obs_id};" -t -A`
#    if [ $exists -eq 1 ]; then
#	under=`psql -h eor-00 mwa_qc -U mwa -c "select count(*) from qs where obsid=${obs_id} and window_x<${thresh} and window_y<${thresh};" -t -A`
#	if [ $under -eq 0 ]; then
#	    echo Obs $obs_id did not meet wedge threshold. Skipping!
#	    status_file=${outdir}/fhd_${version}/metdata/${obs_id}_status.txt
#	    touch $status_file
#	    echo 'WEDGE_FLAGGED 1' > ${status_file}
#	    echo 'COMPLETE 1' >> ${status_file}
#	    exit 1
#	else
#	    echo Obs $obs_id met wedge criteria. Proceeding.
#	fi
#    else
#	echo Did not find wedge stats for obs ${obs_id}. Proceeding.
#    fi
#fi

#ls $outdir > /dev/null # ping the output directory so nfs automounts

module load ghostscript
module load git/2.10.2
module load imagemagick/7.0.7
module load idl
shopt -s expand_aliases; source $IDL/envi53/bin/envi_setup.bash

obsids=("$@")
obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}

idl -IDL_DEVICE ps -quiet -IDL_CPU_TPOOL_NTHREADS $ncores -e wyl_eor_firstpass_versions -args $obs_id $outdir $version 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

