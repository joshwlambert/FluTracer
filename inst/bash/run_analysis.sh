#!/bin/bash
#SBATCH --job-name=FluTracer
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --mem=10GB
#SBATCH --time=05:00:00
#SBATCH --output=%x_%j.log
pwd; hostname; date

subtype=$1
quarantine=$2

echo "Running FluTracer analysis script for: ${subtype}"

module load R/4.4.0

echo "Number of cores available: "

Rscript -e "future::availableCores()"

Rscript inst/scripts/run_analysis.R ${subtype} ${quarantine}

date
