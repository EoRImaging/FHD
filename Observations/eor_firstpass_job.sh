#! /bin/bash
#############################################################################
#Runs one observation at a time in grid engine.  Second level program for 
#running firstpass on the MIT machines. First level program is 
#batch_firstpass.sh
#############################################################################

#$ -V
#$ -N firstpass
#$ -S /bin/bash

# obs_id, outdir, version, and thresh and nslots expected to be passed from qsub call
# in batch_firstpass.sh

echo JOBID ${JOB_ID}
echo TASKID ${SGE_TASK_ID}
obs_id=$(pull_args.py $*)
echo OBSID ${obs_id}

# TODO: check wedge stats and stop if above thresh
if [ "${thresh}" -gt "0" ]; then
    # check db for wedgie
    echo Checking wedgie
    export PGPASSWORD=BowTie	
    # first check that there is an entry
    exists=`psql -h eor-00 mwa_qc -U mwa -c "select count(*) from qs where obsid=${obs_id};" -t -A`
    if [ $exists -eq 1 ]; then
	under=`psql -h eor-00 mwa_qc -U mwa -c "select count(*) from qs where obsid=${obs_id} and window_x<${thresh} and window_y<${thresh};" -t -A`
	if [ $under -eq 0 ]; then
	    echo Obs $obs_id did not meet wedge threshold. Skipping!
	    exit 1
	else
	    echo Obs $obs_id met wedge criteria. Proceeding.
	fi
    else
	echo Did not find wedge stats for obs ${obs_id}. Proceeding.
    fi
fi

ls $outdir > /dev/null # ping the output directory so nfs automounts
/usr/local/bin/idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $nslots -e eor_firstpass_versions -args $obs_id $outdir $version 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

