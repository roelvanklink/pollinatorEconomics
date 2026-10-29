#!/bin/bash
 

#SBATCH --job-name=pollinatorInlaModel1
#SBATCH --time 6:00:00
#SBATCH --mem-per-cpu=50G
#SBATCH --cpus-per-task=4

#SBATCH -o /work/%u/%x-%j-%a.txt






module load foss/2022b R/4.2.2
output_dir=/work/$USER/$SLURM_JOB_NAME

mkdir -p "$output_dir"



Rscript /home/klinkr/inla pollinator global model.R "$output_dir"











