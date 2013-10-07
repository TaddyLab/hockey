#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --output=run.out
#SBATCH --nodes=1
#SBATCH --time=24:00:00

## module load R/3.0
module load openmpi

## ~/R-3.0.1/bin/R CMD BATCH --no-save scrape.R 
## ~/R-3.0.1/bin/R CMD BATCH --no-save buildgoals.R 
~/R-3.0.1/bin/R CMD BATCH --no-save logistic_fits.R 
~/R-3.0.1/bin/R CMD BATCH --no-save logistic_textout.R 
~/R-3.0.1/bin/R CMD BATCH --no-save googlechart.R 

module load git
git commit -a -m "weekly update"
git push
