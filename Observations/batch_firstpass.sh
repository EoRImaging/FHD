#!/bin/bash

#Parse flags for inputs
while getopts ":f:s:e:" option
do
   case $option in
	f) obs_file_name="$OPTARG";;
	s) starting_obs=$OPTARG;;
	e) ending_obs=$OPTARG;;
	\?) echo "Unknown option: Accepted flags are -f (obs_file_name), -s (starting_obs) and -e (ending obs)" 
	    exit 1;;
	:) echo "Missing option argument for input flag"
	   exit 1;;
   esac
done

#Manual shift to the next flag
shift $(($OPTIND - 1))

#Throw error if no obs_id file
if [ -z ${obs_file_name} ]
then
   echo "Need to specify a file with a list of viable observation ids."
   exit 1
fi

nslots=10
mem=30G

#Read the obs file and put into an array
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

#commands for idl
idl_function_path="/nfs/grs1915/ha/nbarry/MWA/development"
idl_function="eor_firstpass_spawn"
export IDL_PATH=$IDL_PATH:+$idl_function_path
idl_e="idl -e $idl_function -args"
version="'nb_firstpass_script_test'"

#Run idl for all specified obs_id using the above commands
for obs_id in "${obs_id_array[@]}"
do
    if [ $obs_id -ge $starting_obs ] && [ $obs_id -le $ending_obs ]
    then
	### set errfile and outfile ###
	echo "Starting observation id $obs_id"
	qsub -l h_vmem=$mem,h_stack=512k -V -v obs_id=$obs_id,nslots=$nslots,version=$version -e $errfile -o $outfile -pe chost $nslots eor_firstpass_job.sh
        #$idl_e $obs_id $version
    fi
done

echo "Done"


