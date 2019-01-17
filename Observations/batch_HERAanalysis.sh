#!/bin/bash
#SBATCH -t 2:00:00
#SBATCH -n 2
#SBATCH --array=0-0:1%20
#SBATCH --mem=60G
#SBATCH --account=jpober-condo
#source activate HERA
module load idl
module load ghostscript
module load imagemagick
module load git

shopt -s expand_aliases; source $IDL/envi53/bin/envi_setup.bash
# Fornax was 1700 on array batch
# Stability analysis should total 15586 observations
#version=HERA_IDR2_Fornax_Decon
#suffix=uvOC
#version=IDR21_firstpassdiffuse_EoR0_minbsl1_phs2_${SLURM_ARRAY_TASK_ID}
#thresh=($(seq 5 5 40))
version=4Pol
#version=CalOpt_bl20_bmthresh0p007_FoV45_PSPEC
#Diffuse_bmthresh05_SI4p6_bl0 
#SkyCal2_nfreq1_bl0_phase0_bmthresh05_GSMPNT

suffix=uv
#data_loc=/users/jkerriga/data/shared/HERA_new/Mar26/
outdir=/users/jkerriga/data/jkerriga/FHD_HERA_ANALYSIS
#outdir=/users/jkerriga/data/jkerriga/FHD_OUTRIGGERS
#outdir=/users/jkerriga/data/jkerriga/FHD_IDR2_1
#FHD_IDR2_1
FHDpath=/users/jkerriga/scratch/FHD
#data_loc=~/data/shared/HERA_new/IDR1/
cd ${outdir} #${data_loc}

#obsids=(zen.2458116.55054.HH.2.402447.uv.uvfits)
#obsids=(zen.2458103.37904.HH.1.098248.uv.uvfits) #original stability obs

#outriggers
#obsids=(zen.2458189.18492.HH.1.370334.uv.uvfits)
#obsids=(zen.2458111.50580.HH.2.030648.uv.uvfits) #zen.2458098.40887.HH.1.195886.uvOCRS.uvfits)

#obsids=(zen.2458*098.*.HH.*.uv.uvfits)
obsids=(zen.2458101.40141.HH.1.224403.uv.uvfits)
#obsids=(zen.grp1.of1.LST.*.${suffix}.uvfits)
#(zen.grp1.of1.xx.LST.1.08784.uvOCRSL.uvfits) #(zen.*.*.xx.HH.${suffix})
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
#"${thresh[SLURM_ARRAY_TASK_ID]}"
