#!/bin/bash

####################################################
#
# SIM_PIPE_SLURM.SH

#  A top-level script for running the simulation wrapper, adapted from pipe_dream.sh.
# 	 Using sim_density_wrapper.pro

# Required input arguments are obs_file_name (-f /path/to/obsfile) and version
# (-v yourinitials_jackknife_test)
#
# Optional input arguments are: starting_obs (-s 1061311664) which is defaulted to the beginning
# obsid of the specified file, ending_obs (-e 1061323008) which is defaulted to the ending obsid
# of the specified file, outdir (-o /path/to/output/directory) which is defaulted to 
# /nfs/mwa-09/r1/djc/EoR2013/Aug23, priority (-p -10) which is defaulted to zero but can range from
# -20 (higher priority) to 20 (lower priority), wallclock_time (-w 08:00:00) which is defaulted to 
# 4 hours for a typical firstpass run, nslots (-n 10) which is defaulted to 10 for a typical IDL
# job, mem (-m 4G) which is defaulted to 4 Gigabytes per slot for a typical firstpass run, and
# thresh (-t 1) which is defaulted to 1 to tell the code to not look for a threshold from wedge
# statistics.
#
# WARNING!
# Terminal will hang as it waits for jobs to finish, and closing the terminal will kill any 
# remaining jobs! To run in the background, run: 
# nohup ./pipe_slurm.sh -f /path/to/obsfile -v yourinitials_jackknife_test > /path/to/your/output/log/file.txt &
#
####################################################

#Clear input parameters
unset obs_file_name
unset starting_obs
unset ending_obs
unset outdir
unset version
unset resubmit_list
unset resubmit_index

module load idl
shopt -s expand_aliases; source $IDL/envi53/bin/envi_setup.bash

#######Gathering the input arguments and applying defaults if necessary

#Parse flags for inputs
while getopts ":f:s:e:o:v:i:p:w:n:m:q:t" option
do
   case $option in
	f) obs_file_name="$OPTARG";;	#text file of observation id's
	s) starting_obs=$OPTARG;;	#starting observation in text file for choosing a range
	e) ending_obs=$OPTARG;;		#ending observation in text file for choosing a range
        o) outdir=$OPTARG;;		#output directory for FHD output folder
        v) version=$OPTARG;;		#Case for variation_versions
	i) sim_id=$OPTARG;;		#sim_id creates fhd folder (Example: nb_foo creates folder named fhd_nb_foo)
	w) wallclock_time=$OPTARG;;	#Time for execution in slurm
	n) ncores=$OPTARG;;		#Number of cores for slurm
	m) mem=$OPTARG;;		#Memory per node for slurm
    q) qos=$OPTARG;;        # Quality of service for Oscar
	t) firstpass_only=1;;	#Firstpass only, or also do power spectrum?
	\?) echo "Unknown option: Accepted flags are -f (obs_file_name), -s (starting_obs), -e (ending obs), -o (output directory), "
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

#Throw error if no obs_id file.
if [ -z ${obs_file_name} ]; then
   echo "Need to specify a full filepath to a list of viable observation ids."
   exit 1
fi

obs_file_name=$(readlink -f $obs_file_name)   # Get the complete absolute path

if [ -z ${qos} ]
then
    qos='jpober-condo'
fi

#firstpass_only specification
if [ -z ${firstpass_only} ]
then
     firstpass_only=0
else
     firstpass_only=1
fi

#Set default output directory if one is not supplied and update user
if [ -z ${outdir} ]
then
    outdir=/gpfs/data/jpober/alanman/FHD_out
    echo Using default output directory: $outdir
else
    #strip the last / if present in output directory filepath
    outdir=${outdir%/}
    echo Using output directory: $outdir
fi

#Use default version if not supplied.
if [ -z ${version} ]; then
   echo Please specify a version, e.g, yourinitials_test
   exit 1
fi


#if grep -q \'${version}\' ${FHDpath}Observations/eor_firstpass_versions.pro
#then
#    echo Using version $version
#else
#    echo Version \'${version}\' was not found in ${FHDpath}Observations/eor_firstpass_versions.pro
#    exit 1
#fi

#Set typical wallclock_time for standard FHD firstpass if not set.
if [ -z ${wallclock_time} ]; then
    wallclock_time=4:00:00
fi
#Set typical cores needed for standard FHD firstpass if not set.
if [ -z ${ncores} ]; then
    ncores=15
fi
#Set typical memory needed for standard FHD firstpass if not set.
if [ -z ${mem} ]; then
    mem=30G
fi
if [ -z ${thresh} ]; then
    # if thresh is not set, set it to -1 which will cause it to not check for a window power
    thresh=-1
fi

if [ -z ${sim_id} ]; then
   echo "Warning: sim_id not specified. Using the version string"
   sim_id=${version}
   #Make directory if it doesn't already exist
   mkdir -p ${outdir}/fhd_${version}
   mkdir -p ${outdir}/fhd_${version}/grid_out
   echo Output located at ${outdir}/fhd_${version}
   fhddir=${outdir}/fhd_${version}
else
   #Make directory if it doesn't already exist
   mkdir -p ${outdir}/fhd_${version}_${sim_id}
   mkdir -p ${outdir}/fhd_${version}_${sim_id}/grid_out
   echo Output located at ${outdir}/fhd_${version}_${sim_id}
   fhddir=${outdir}/fhd_${version}_${sim_id}
fi

#Read the obs file and put into an array, skipping blank lines if they exist.
# If the file doesn't exist, treat the obs_file_name as a single obsid
if [ ! -e "$obs_file_name" ]
then
     obs_id_array=($obs_file_name)
     max=$obs_file_name
     min=$obs_file_name 
else
     i=0
     while read line
     do
        if [ ! -z "$line" ]; then
           obs_id_array[$i]=$line
           i=$((i + 1))
        fi
     done < "$obs_file_name"
    ##Find the max and min of the obs id array
    max=${obs_id_array[$((i-1))]}
    min=${obs_id_array[0]}
fi

#If minimum not specified, start at minimum of obs_file
if [ -z ${starting_obs} ]
then
   echo "Starting observation not specified: Starting at minimum of "$(basename $obs_file_name)""
   starting_obs=$min
fi

#If maximum not specified, end at maximum of obs_file
if [ -z ${ending_obs} ]
then
   echo "Ending observation not specified: Ending at maximum of "$(basename $obs_file_name)""
   ending_obs=$max
fi

#Create a list of observations using the specified range, or the full observation id file. 

unset good_obs_list
startflag=0
endflag=0

for obs_id in "${obs_id_array[@]}"; do 
     if [ $obs_id == $starting_obs ]; then 
     	startflag=1
     fi
     if [ $startflag -eq 1 ] && [ $endflag -eq 0 ]; then
     	good_obs_list+=($obs_id)
     fi
     if [ $obs_id == $ending_obs ]; then
    	endflag=1
     fi
done	

echo ${good_obs_list[@]}


#######End of gathering the input arguments and applying defaults if necessary


#######Submit the job and wait for output

#Find the number of obsids to run in array
nobs=${#good_obs_list[@]}

#### !!! The -w flag chooses a specific node.
message=$(sbatch -A $qos -N 1-2 --mem=$mem -t ${wallclock_time} -n ${ncores} --array=0-$(( $nobs - 1 ))%20 --export=ncores=$ncores,outdir=$outdir,version=$version,sim_id=$sim_id,thresh=$thresh -o ${fhddir}/grid_out/array_sim-%A_%a.out -e ${fhddir}/grid_out/array_sim-%A_%a.err ${FHDpath}simulation/simulation_wrappers/eor_simulation_slurm_job.sh ${good_obs_list[@]})

#echo $message

#Run the command
message=($message)

echo ${message[@]}
#Gather the slurm id from the job for later use
id=`echo ${message[3]}`

while [ `myq | grep $id | wc -l` -ge 1 ]; do
    sleep 10
done

########End of submitting the firstpass job and waiting for output

if [ $firstpass_only -eq 1 ]; then
	curdir=`pwd -P`
	cd ${fhddir}
	uvconvert.py -o miriad
	cd $curdir
	exit 0
fi

# Submit a job to convert model visibilities to uvfits and MIRIAD formats
curdir=`pwd -P`
cd ${fhddir}
uvconvert.py -o miriad
cd $curdir

### NOTE this only works if idlstartup doesn't have any print statements (e.g. healpix check)
PSpath=$(idl -e 'print,rootdir("eppsilon")')

if [ ! -z ${ending_obs} ]; then
    echo "Ending obs: "${ending_obs}
    if [ ! -z ${starting_obs} ]; then	
        echo 'Running ps with both starting and ending obs'
	    ${PSpath}ps_wrappers/ps_slurm.sh -s ${starting_obs} -e ${ending_obs} -f $obs_file_name -d ${fhddir} -w ${wallclock_time} -m ${mem}
    else
        echo 'Running ps with both ending obs'
	    ${PSpath}ps_wrappers/ps_slurm.sh -e ${ending_obs} -f $obs_file_name -d ${fhddir} -w ${wallclock_time} -m ${mem}
    fi
elif [ ! -z ${starting_obs} ]; then
        echo 'Running ps with both starting obs'
	    ${PSpath}ps_wrappers/ps_slurm.sh -s ${starting_obs} -f $obs_file_name -d ${fhddir} -w ${wallclock_time} -m ${mem}
else
    ${PSpath}ps_wrappers/ps_slurm.sh -f $obs_file_name -d ${fhddir} -w ${wallclock_time} -m ${mem}
fi

echo "Cube integration and PS submitted"
