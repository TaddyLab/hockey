#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --output=run.out
#SBATCH --nodes=1
#SBATCH --time=24:00:00

## module load R/3.0
module load openmpi

~/R-3.0.1/bin/R CMD BATCH --no-save code/blog/scrape.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/blog/buildgoals.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/blog/glfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/blog/rlfit.R 
~/R-3.0.1/bin/R CMD BATCH --no-save code/blog/glchart.R 

module load git
git add data/roster.RData data/nhldesign.rda results/*effects.csv
git commit -m "weekly update"
git push
