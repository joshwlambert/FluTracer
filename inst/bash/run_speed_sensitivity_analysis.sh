#!/bin/bash
#SBATCH --job-name=FluTracer
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem=10GB
#SBATCH --time=05:00:00
#SBATCH --output=speed_sensitivity_analysis.log
pwd; hostname; date

echo "Running FluTracer speed-sensitivty analysis script."

module load R/4.4.0

Rscript inst/scripts/run_speed_sensitivity_analysis.R

date
