#!/bin/bash
#SBATCH --job-name=FluTracer
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --mem=10GB
#SBATCH --time=05:00:00
#SBATCH --output=%x_%j.log
pwd; hostname; date

subtype=$1

echo "Running FluTracer droplet transmission analysis script for: ${subtype}"

module load R/4.4.0

echo "Number of cores available: "

Rscript -e "future::availableCores()"

Rscript inst/scripts/run_droplet_analysis.R ${subtype}

date
