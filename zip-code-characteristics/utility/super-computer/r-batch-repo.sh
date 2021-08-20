#!/bin/bash
#
#SBATCH --partition=normal
#SBATCH --ntasks=1
#SBATCH --mem=10240
#SBATCH --output=utility/super-computer/output/repo.txt
#SBATCH --error=utility/super-computer/error/repo.txt
#SBATCH --time=02:00:00
#SBATCH --job-name=zip-code-characteristics
#SBATCH --mail-user=wibeasley@hotmail.com
#SBATCH --mail-type=ALL
#SBATCH --chdir=/home/wbeasley/zip-code-characteristics
#
#################################################
module load Pandoc/2.5
module load R/4.0.2-foss-2020a

Rscript utility/super-computer/repo.R > utility/super-computer/output/repo-direct.txt
