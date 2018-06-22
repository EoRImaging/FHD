#!/bin/bash
#SBATCH -t 4:00:00
#SBATCH -n 3
####SBATCH --array=0-1:1
#SBATCH --mem=120G
#SBATCH --account=jpober-condo
#source activate HERA
module load idl
module load ghostscript
module load imagemagick
module load git

shopt -s expand_aliases; source $IDL/envi53/bin/envi_setup.bash

#version=HERA_IDR2_Fornax_Decon
#suffix=uvOC
version=Decon 
#version=Diffuse_test
suffix=uvN
data_loc=/users/jkerriga/data/shared/HERA_new/Mar26/
outdir=/users/jkerriga/data/jkerriga/HERAFHD
FHDpath=/users/jkerriga/scratch/FHD
#data_loc=~/data/shared/HERA_new/IDR1/
cd ${outdir} #${data_loc}

obsids=(zen.2458107.33430.HH.uvOCRN.uvfits) #decon & diffuse
#obsids=(zen.2458186.12526.HH.uvN.uvfits)
#obsids=(zen.2458107.46852.HH.uvOC.uvfits)
#obsids=(zen.grp1.of1.LST.0.81849.uvOCRSL.uvfits) #(zen.*.*.xx.HH.${suffix})
obs=${obsids[$SLURM_ARRAY_TASK_ID]}
#obs_base=(${obs//.xx.HH.${suffix}/ })


#if [ ! -f ${outdir}/${obs_base}'.HH.'${suffix}'.uvfits' ]; then
#    echo "UVFITS doesn't exist."
#    echo 'Combining polarizarions and converting miriad file to uvfits.'
#    python ~/scratch/FHD_tools/combine_pols.py $obs
#    #FHDpath=/users/jkerriga/FHD
#    echo 'Moving '${obs}'.uvfits to '$outdir
#    obs_uvfits=(${obs//.xx.HH.${suffix}/ })
#    obs_uvfits=$obs_uvfits'.HH.'${suffix}'.uvfits' 
#    mv -f $obs_uvfits $outdir
#else
#    obs_uvfits=(${obs//.xx.HH.${suffix}/ })
#    obs_uvfits=$obs_uvfits'.HH.'${suffix}'.uvfits' 
#fi

#IDR1_Subtract
ncores=3
#Make directory if it doesn't already exist
mkdir -p ${outdir}/fhd_${version}
mkdir -p ${outdir}/fhd_${version}/grid_out
echo Output located at ${outdir}/fhd_${version}

#######Submit the firstpass job and wait for output

#Find the number of obsids to run in array
#nobs=${#good_obs_list[@]}
echo 'Submitting job to queue.'
#cd /users/jkerriga/data/jkerriga/HERAFHD
#obsids=(*.uvfits)
#obsids=(${obsids[@]//.uvfits/ })
#obs_id=${obsids[$SLURM_ARRAY_TASK_ID]}
obs_id=$obs #obs_uvfits
echo $obs_id
cd /users/jkerriga/scratch/FHD/Observations
#cd /users/jkerriga/FHD/Observations
#/usr/local/bin/idl 
idl -IDL_DEVICE ps -quiet -IDL_CPU_TPOOL_NTHREADS $ncores -e hera_wrapper -args $obs_id $outdir $version
