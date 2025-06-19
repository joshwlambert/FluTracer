#!/bin/bash
#SBATCH --job-name=install_FluTracer
#SBATCH --ntasks=1
#SBATCH --mem=5GB
#SBATCH --time=01:00:00
#SBATCH --output=install_FluTracer.log
pwd; hostname; date

ml R/4.4.0
Rscript -e "remotes::install_github('joshwlambert/FluTracer')"
