#!/bin/bash

#Clear input parameters
unset obs_file_name
unset starting_obs
unset ending_obs
unset outdir
unset version
#Parse flags for inputs
while getopts ":f:s:e:o:v:" option
do
   case $option in
	f) obs_file_name="$OPTARG";;
	s) starting_obs=$OPTARG;;
	e) ending_obs=$OPTARG;;
        o) outdir=$OPTARG;;
        v) version=$OPTARG;;
	\?) echo "Unknown option: Accepted flags are -f (obs_file_name), -s (starting_obs) and -e (ending obs)" 
	    exit 1;;
	:) echo "Missing option argument for input flag"
	   exit 1;;
   esac
done

echo starting obs $starting_obs
echo ending obs $ending_obs

#Manual shift to the next flag
shift $(($OPTIND - 1))

#Throw error if no obs_id file
if [ -z ${obs_file_name} ]
then
   echo "Need to specify a file with a list of viable observation ids."
   exit 1
fi

#Set default output directory if one is not supplied
if [ -z ${outdir} ]; then
    outdir=/nfs/mwa-09/r1/djc/EoR2013/Aug23
    echo Using default output directory: $outdir
fi
#Use default version if not supplied
if [ -z ${version} ]; then
    version=test_batch_firstpass
    echo Using default FHD version: $version
fi

### Should make these options
nslots=10
mem=3G ### NOTE this is PER CORE!

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
#idl_function_path="/nfs/grs1915/ha/nbarry/MWA/development"
#idl_function="eor_firstpass_spawn"
#export IDL_PATH=$IDL_PATH:+$idl_function_path
#idl_e="idl -e $idl_function -args"
#version="'nb_firstpass_script_test'"
FHDpath=$(idl -e 'print,rootdir("fhd")') ### NOTE this only works if idlstartup doesn't have any print statements (e.g. healpix check)

#Run idl for all specified obs_id using the above commands
unset good_obs_list
for obs_id in "${obs_id_array[@]}"; do
    if [ $obs_id -ge $starting_obs ] && [ $obs_id -le $ending_obs ]; then
	good_obs_list+=($obs_id)
    fi
done

for obs_id in "${obs_id_array[@]}"
do
    if [ $obs_id -ge $starting_obs ] && [ $obs_id -le $ending_obs ]
    then
	### Might need to mkdir before using these files ###
	errfile=${outdir}/fhd_${version}/${obs_id}_err.log
	outfile=${outdir}/fhd_${version}/${obs_id}_out.log
	echo "Starting observation id $obs_id"
	qsub -P FHD -l h_vmem=$mem,h_stack=512k -V -v obs_id=$obs_id,nslots=$nslots,outdir=$outdir,version=$version -e $errfile -o $outfile -pe chost $nslots ${FHDpath}Observations/eor_firstpass_job.sh
        #$idl_e $obs_id $version
    fi
done

echo "Done"


