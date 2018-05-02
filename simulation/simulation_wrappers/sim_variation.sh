#!/bin/bash

####################################################
#
# SIM_VARIATION.SH

#  A top-level script for running the simulation wrapper, adapted from pipe_dream.sh.
#

#
####################################################

#Clear input parameters
unset obs_id
unset outdir
unset version
unset resubmit_list
unset resubmit_index

#######Gathering the input arguments and applying defaults if necessary

#Parse flags for inputs
while getopts ":f:p:o:v:w:n:m:t" option
do
   case $option in
	f) obs_id="$OPTARG";;	#Obs_ID
	p) param_var="$OPTARG";; 	  # String specifying values for a given parameter
    o) outdir=$OPTARG;;		#output directory for FHD output folder
    v) version=$OPTARG;;		#FHD folder name and case for variation_versions
					#Example: nb_foo creates folder named fhd_nb_foo
	w) wallclock_time=$OPTARG;;	#Time for execution in slurm
	n) cores=$OPTARG;;		#Number of cores for slurm
	m) mem=$OPTARG;;		#Memory per node for slurm
	t) firstpass_only=1;;	#Firstpass only, or also do power spectrum?
	\?) echo "Unknown option: Accepted flags are -f (obs_id), -o (output directory), "
	    echo "-v (version input for FHD), -w (wallclock time in slurm), -n (number of cores to use),"
	    echo "and -m (memory per core for slurm)." 
	    exit 1;;
	:) echo "Missing option argument for input flag"
	   exit 1;;
   esac
done

#Manual shift to the next flag.
shift $(($OPTIND - 1))

#Specify the FHD file path that is used in IDL (generally specified in idl_startup)
FHDpath=$(idl -e 'print,rootdir("fhd")') ### NOTE this only works if idlstartup doesn't have any print statements (e.g. healpix check)
## It's slow:
#FHDpath="/users/alanman/FHD/"

#Throw error if no obs_id.
if [ -z ${obs_id} ]; then
   echo "Need to specify an obs_id."
   exit 1
fi

#firstpass_only specification
if [ -z ${firstpass_only} ]
then
     firstpass_only=0
else
     firstpass_only=1
fi

#set default output directory if one is not supplied and update user
if [ -z ${outdir} ]
then
    outdir=/gpfs/data/jpober/alanman/FHD_out
    echo using default output directory: $outdir
else
    #strip the last / if present in output directory filepath
    outdir=${outdir%/}
    echo using output directory: $outdir
fi

#Need a version.
if [ -z ${version} ]; then
   echo please specify a version
   exit 1
fi

#set typical wallclock_time for standard fhd firstpass if not set.
if [ -z ${wallclock_time} ]; then
    wallclock_time=10:00:00
fi
#set typical cores needed for standard fhd firstpass if not set.
if [ -z ${ncores} ]; then
    ncores=10
fi
#set typical memory needed for standard fhd firstpass if not set.
if [ -z ${mem} ]; then
    mem=30g
fi
if [ -z ${thresh} ]; then
    # if thresh is not set, set it to -1 which will cause it to not check for a window power
    thresh=-1
fi

if [ -z ${param_var} ]; then
    #These strings have the form "parameter=value1,value2,value3,value4...]"
    echo "Parameter variation string is required for this script."
    exit 1
fi

#make directory if it doesn't already exist
mkdir -p ${outdir}/fhd_${version}
mkdir -p ${outdir}/fhd_${version}/grid_out
echo output located at ${outdir}/fhd_${version}


### Parse the parameter string to get the number of array jobs to run:

njobs=$(grep -o "," <<< "${param_var}" | wc -l)
njobs=$(( $njobs + 1 ))  # Counting commas only gives njobs-1, so correct this

### Replace commas with underscores in param_var
param_var=${param_var//,/_}


#######End of gathering the input arguments and applying defaults if necessary


#######Submit the job and wait for output


#### !!! The -w flag chooses a specific node.
message=$(sbatch  --mem=$mem -t ${wallclock_time} -n ${ncores} --array=0-$(( $njobs - 1 )) --export=ncores=$ncores,outdir=$outdir,version=$version,thresh=$thresh,param_var=$param_var -o ${outdir}/fhd_${version}/grid_out/array_sim-%A_%a.out -e ${outdir}/fhd_${version}/grid_out/array_sim-%A_%a.err ${FHDpath}simulation/simulation_wrappers/parameter_variation_job.sh ${obs_id})

#echo $message

#Run the command
message=($message)


echo ${message[@]}
#Gather the slurm id from the job for later use
id=`echo ${message[3]}`

exit 0

while [ `myq | grep $id | wc -l` -ge 1 ]; do
    sleep 10
done


########End of submitting the firstpass job and waiting for output

if [ $firstpass_only -eq 1 ]; then
	curdir=`pwd -P`
	cd ${outdir}/fhd_${version}
	uvconvert.py -o miriad
	cd $curdir
	exit 0
fi


# Submit a job to convert model visibilities to uvfits and MIRIAD formats
curdir=`pwd -P`
cd ${outdir}/fhd_${version}
uvconvert.py -o miriad
cd $curdir


### NOTE this only works if idlstartup doesn't have any print statements (e.g. healpix check)
PSpath=$(idl -e 'print,rootdir("eppsilon")')


${PSpath}ps_wrappers/ps_slurm.sh -f $obs_id -d $outdir/fhd_$version -w ${wallclock_time} -m ${mem}


echo "PS submitted"
