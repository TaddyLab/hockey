#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --output=run.out
#SBATCH --nodes=1
#SBATCH --time=24:00:00

## module load R/3.0
module load openmpi

~/R-3.0.1/bin/R CMD BATCH --no-save code/scrape.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/buildgoals.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/glfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/rlfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/glchart.R 

module load git
git add data/roster.RData data/nhldesign.rda results/*effects.csv
git commit -m "weekly update"
git push
