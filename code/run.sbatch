#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --output=run.out
#SBATCH --nodes=1
#SBATCH --time=24:00:00

## module load R/3.0
module load openmpi

~/R-3.0.1/bin/R CMD BATCH --no-save scrape.R 
~/R-3.0.1/bin/R CMD BATCH --no-save buildgoals.R 
~/R-3.0.1/bin/R CMD BATCH --no-save glfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save ltlfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save glchart.R 

module load git
git add data/nhldesign.rda results/*effects.txt
git commit -m "weekly update"
git push
