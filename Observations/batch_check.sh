#! /bin/bash
##############
#Script to check status of firstpass run on MIT cluster.
#Specific to the MIT cluster!
#Checks status.txt files for each obs, resubmits jobs if they did not complete successfully.
##############

#$ -V
#$ -N fp_check
#$ -S /bin/bash

obslist=$*

echo JOBID ${JOB_ID}
echo OBSLIST $obslist

metadir=${outdir}/fhd_${version}/metadata
ls $outdir > /dev/null # ping output directory so nfs automounts
cd ${outdir}/fhd_${version}

unset resubmit_list
for obs in $obslist; do
    file=${metadir}/${obs}_status.txt
    # first check if the status file even exists.
    if [ ! -f ${file} ]; then
	echo Obs $obs did not finish successfully. (no file).
	resubmit_list+=($obs)
	rm */${obs}*
	continue
    fi
    # Read in last line
    last_tag=`cut -f1 ${file} | tail -1`
    if [ "${last_tag}" != "COMPLETE" ]; then
	echo Obs $obs did not finish successfully. (no complete line).
	resubmit_list+=($obs)
	rm */${obs}*
	continue
    fi
    complete=`cut -f2 ${file} | tail -1`
    if [ "${complete}" -ne "1" ]; then
	echo Obs $obs did not finish successfully. (complete is zero).
	resubmit_list+=($obs)
	rm */${obs}*
	continue
    fi
    # if we've gotten this far, it must have succeeded.
    echo Obs $obs finished successfully. Jolly good.
done

nobs=${#resubmit_list[@]}

if [ "${nobs}" -gt 0 ]; then
    # resubmit
    echo Resubmitting the baddies.
    ssh eor-00 qsub -p $priority -P FHD -l h_vmem=$mem,h_stack=512k,h_rt=${wallclock_time} -V -v nslots=$nslots,outdir=$outdir,version=$version -e ${outdir}/fhd_${version}/grid_out -o ${outdir}/fhd_${version}/grid_out -t 1:${nobs} -pe chost $nslots ${FHDpath}Observations/eor_firstpass_job.sh ${resubmit_list[@]}
else
    echo Congratulations, your run finished successfully.
fi


