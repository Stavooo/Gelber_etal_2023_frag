#!/bin/bash -e

#SBATCH --job-name=frag_red_hab
#SBATCH --mail-user=stag86@zedat.fu-berlin.de
#SBATCH --mail-type=end
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=700
#SBATCH --time=2:00:00
#SBATCH --qos=standard
#SBATCH --array=1-10

module add R

Rscript "cluster_model_run.R" ${SLURM_ARRAY_TASK_ID} ${SLURM_CPUS_PER_TASK}
