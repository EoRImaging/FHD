#!/bin/bash

#####################################################################################
#A script to delete specified FHD directories, and move some metadata and the 
#relatevely small ps directory to the /old directory for posteriety.
#
#version is a necessary input. It should be the version name of the directory
#you would like to delete, and should be in the form of "nb_version_test"
#
#outdir and directory of optional flags. Outdir specifies where to put posteriety
#data, and if not supplied, it will move the data to the old directory located
#at the typical fhd output site. Directory specifies the directory to the fhd
#directory that you would like to delete, and if not supplied, it will 
#choose the typical fhd output site. 
#####################################################################################

#Clear input parameters
unset outdir
unset version

#Parse flags for inputs
while getopts ":o:v:d:" option
do
   case $option in
        o) outdir=$OPTARG;;		#output directory for FHD output folder, default is the old directory
        v) version=$OPTARG;;		#FHD folder name to be removed
	d) directory=$OPTARG;;		#Optional FHD folder directory, default is the Aug23 directory
	\?) echo "Unknown option: Accepted flags are -o (optional output directory if not the old directory), "
	    echo "-v (version input for FHD), and -d  (optional FHD folder directory if not in Aug23 directory)"
	    exit 1;;
	:) echo "Missing option argument for input flag"
	   exit 1;;
   esac
done

echo Deleting $version

#Manual shift to the next flag.
shift $(($OPTIND - 1))


#Set default output directory if one is not supplied.
if [ -z ${outdir} ]; then
    outdir=/nfs/mwa-09/r1/djc/EoR2013/Aug23/old
    echo Using default output directory: $outdir
else
    #strip the last / if present in output directory filepath
    outdir=${outdir%/}
    echo Using output directory: $outdir
fi
#Use default version if not supplied.
if [ -z ${version} ]; then
    echo Please specify a version to delete
    exit 1
fi
#Set default fhd directory if one is not supplied.
if [ -z ${directory} ]; then
    directory=/nfs/mwa-09/r1/djc/EoR2013/Aug23
    echo Using default fhd directory: $directory
else
    #strip the last / if present in output directory filepath
    directory=${directory%/}
    echo Using fhd directory: $directory
fi
#Check to see if the supplied folder to delete exists.
if [ ! -d "$directory/fhd_$version" ]; then
    echo "The supplied directory to delete ($directory/fhd_$version) doesn't exist. Exiting"
    exit 1
fi
#Check to see if the directory already exists in the old directory
if [ -d "${outdir}/fhd_${version}" ]; then
    echo ${outdir}/fhd_${version} already exists. Exiting.
    exit 1
fi

#This helps to make a little lee-way room for making a new directory and moving wanted data,
#which is necessary sometimes if you're stuck and want to move to an old directory on the
#same disk.
rm ${directory}/fhd_${version}/Healpix -r

#Make the directory in which to put the old data
mkdir ${outdir}/fhd_${version}

#Move the power spectra results, the metadata, and the output images into the old folder
mv ${directory}/fhd_${version}/ps ${outdir}/fhd_${version}/ps
mv ${directory}/fhd_${version}/metadata ${outdir}/fhd_${version}/metadata
mv ${directory}/fhd_${version}/output_images ${outdir}/fhd_${version}/output_images

#Delete the rest of the unwanted data
rm ${directory}/fhd_${version} -r

echo "Deletion complete"


