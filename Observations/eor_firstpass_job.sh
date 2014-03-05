#! /bin/bash
#run firstpass on single observation
#$ -V
#$ -N firstpass
#$ -S /bin/bash

# obs_id, outdir, version and nslots expected to be passed from qsub call

echo JOBID ${JOB_ID}

/usr/local/bin/idl -IDL_DEVICE ps -IDL_CPU_TPOOL_NTHREADS $nslots -e eor_firstpass_spawn -args $obs_id $outdir $version