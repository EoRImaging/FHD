#! /bin/bash
#run firstpass on single observation
#$ -V
#$ -N firstpass
#$ -S /bin/bash

# obs_id, outdir, version and nslots expected to be passed from qsub call

echo JOBID ${JOB_ID}
echo obsid ${obs_id}

/usr/local/bin/idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $nslots -e eor_firstpass_spawn -args $obs_id $outdir $version 

if [ $? -eq 0 ]
then
    echo "Finished"
    exit 0
else
    echo "Job Failed"
    exit 1
fi

