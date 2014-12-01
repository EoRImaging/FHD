#!/bin/bash

#####################################################################################
#This is the top level script for processing FHD firstpass job on the cluster.
#
#NOTE: print statements must be turned off in idl_startup file (e.g. healpix check)
#
#What is needed: a file with observation id's separated by newlines, and an output 
#                  directory in which to put FHD output folder.
#What is optional: specified starting and ending observation id's to choose a range
#		   of observations within the obs text file, and the version 
#                  of which to save FHD under and of which settings to choose from 
#		   within eor_firstpass_versions (highly recommended to have)
#####################################################################################

#Clear input parameters
unset obs_file_name
unset starting_obs
unset ending_obs
unset outdir
unset version

#Parse flags for inputs
while getopts ":f:s:e:o:v:p:w:n:m:" option
do
   case $option in
	f) obs_file_name="$OPTARG";;	#text file of observation id's
	s) starting_obs=$OPTARG;;	#starting observation in text file for choosing a range
	e) ending_obs=$OPTARG;;		#ending observation in text file for choosing a range
        o) outdir=$OPTARG;;		#output directory for FHD output folder
        v) version=$OPTARG;;		#FHD folder name and case for eor_firstpass_versions
					#Example: nb_foo creates folder named fhd_nb_foo
        p) priority=$OPTARG;;		#priority level for grid engine qsub 
	w) wallclock_time=$OPTARG;;	#Time for execution in grid engine
	n) nslots=$OPTARG;;		#Number of slots for grid engine
	m) mem=$OPTARG;;		#Memory per core for grid engine
	t) thresh=$OPTARG;;		#Wedge threshold to use to determine whether or not to run
	\?) echo "Unknown option: Accepted flags are -f (obs_file_name), -s (starting_obs), -e (ending obs), -o (output directory), "
	    echo "-v (version input for FHD), -p (priority in grid engine), -w (wallclock time in grid engine), -n (number of slots to use),"
	    echo "and -m (memory per core for grid engine)." 
	    exit 1;;
	:) echo "Missing option argument for input flag"
	   exit 1;;
   esac
done

echo starting obs $starting_obs
echo ending obs $ending_obs

#Manual shift to the next flag.
shift $(($OPTIND - 1))


#Throw error if no obs_id file.
if [ -z ${obs_file_name} ]; then
   echo "Need to specify a file with a list of viable observation ids."
   exit 1
fi
#Set default output directory if one is not supplied.
if [ -z ${outdir} ]; then
    outdir=/nfs/mwa-09/r1/djc/EoR2013/Aug23
    echo Using default output directory: $outdir
fi
#Use default version if not supplied.
if [ -z ${version} ]; then
    version=test_batch_firstpass
    echo Using default FHD version: $version
fi
#Default priority if not set.
if [ -z ${priority} ]; then
    priority=0
fi
#Set typical wallclock_time for standard FHD firstpass if not set.
if [ -z ${wallclock_time} ]; then
    wallclock_time=04:00:00
fi
#Set typical slots needed for standard FHD firstpass if not set.
if [ -z ${nslots} ]; then
    nslots=10
fi
#Set typical memory needed for standard FHD firstpass if not set.
if [ -z ${mem} ]; then
    mem=4G
fi
if [ -z ${thresh} ]; then
    # if thresh is not set, set it to -1 which will cause it to not check for a window power
    thresh=-1
fi

echo Setting priority = $priority

#Make directory if it doesn't already exist
mkdir -p ${outdir}/fhd_${version}
mkdir -p ${outdir}/fhd_${version}/grid_out
echo Output located at ${outdir}/fhd_${version}

#Read the obs file and put into an array.
i=0
while read line
do
   obs_id_array[$i]=$line
   i=$((i + 1))
done < "$obs_file_name"

#Find the max and min of the obs id array
max=${obs_id_array[0]}
min=${obs_id_array[0]}

for obs_id in "${obs_id_array[@]}"
do
   #Update max if applicable
   if [[ "$obs_id" -gt "$max" ]]
   then
	max="$obs_id"
   fi

   #Update min if applicable
   if [[ "$obs_id" -lt "$min" ]]
   then
	min="$obs_id"
   fi
done

#If minimum not specified, start at minimum of obs_file
if [ -z ${starting_obs} ]
then
   echo "Starting observation not specified: Starting at minimum of $obs_file_name"
   starting_obs=$min
fi

#If maximum not specified, end at maximum of obs_file
if [ -z ${ending_obs} ]
then
   echo "Ending observation not specified: Ending at maximum of $obs_file_name"
   ending_obs=$max
fi

#Create a list of observations using the specified range, or the full observation id file. 
unset good_obs_list
for obs_id in "${obs_id_array[@]}"; do
    if [ $obs_id -ge $starting_obs ] && [ $obs_id -le $ending_obs ]; then
	good_obs_list+=($obs_id)
    fi
done

#Specify the FHD file path that is used in IDL (generally specified in idl_startup)
FHDpath=$(idl -e 'print,rootdir("fhd")') ### NOTE this only works if idlstartup doesn't have any print statements (e.g. healpix check)

#Begin submitting job loop

nobs=${#good_obs_list[@]}

message=$(qsub -p $priority -P FHD -l h_vmem=$mem,h_stack=512k,h_rt=${wallclock_time} -V -v nslots=$nslots,outdir=$outdir,version=$version,thresh=$thresh -e ${outdir}/fhd_${version}/grid_out -o ${outdir}/fhd_${version}/grid_out -t 1:${nobs} -pe chost $nslots ${FHDpath}Observations/eor_firstpass_job.sh ${good_obs_list[@]})
message=($message)
id=`echo ${message[2]} | cut -f1 -d"."`

qsub -hold_jid $id -l h_vmem=1G,h_stack=512k,h_rt=00:05:00 -V -v nslots=$nslots,outdir=$outdir,version=$version,FHDpath=$FHDpath,mem=$mem,wallclock_time=${wallclock_time},priority=$priority,thresh=$thresh -e ${outdir}/fhd_${version}/grid_out -o ${outdir}/fhd_${version}/grid_out -pe chost 1 ${FHDpath}Observations/batch_check.sh ${good_obs_list[@]}

echo "Done"


