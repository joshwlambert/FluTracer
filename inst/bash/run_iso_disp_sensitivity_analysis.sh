#!/bin/bash
#SBATCH --job-name=FluTracer
#SBATCH --ntasks=16
#SBATCH --nodes=1
#SBATCH --mem=10GB
#SBATCH --time=05:00:00
#SBATCH --output=%x_%j.log
pwd; hostname; date

echo "Running FluTracer analysis isolation R0 and dispersion sensitivity script"

module load R/4.4.0

echo "Number of cores available: "

Rscript -e "future::availableCores()"

Rscript inst/scripts/run_iso_disp_sensitivity_analysis.R

date
