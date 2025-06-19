#!/bin/bash
#SBATCH --job-name=FluTracer
#SBATCH --ntasks=1
#SBATCH --mem=5GB
#SBATCH --time=01:00:00
#SBATCH --output=FluTracer.log
pwd; hostname; date

echo "Running FluTracer analysis script"

module load R/4.4.0

Rscript inst/scripts/run_analysis.R

date
