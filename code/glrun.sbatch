#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive
#SBATCH --output=glrun.out
#SBATCH --nodes=1
#SBATCH --time=01:00:00

#R CMD BATCH --no-save scrape.R 
R CMD BATCH --no-save glbuild.R 
R CMD BATCH --no-save glfit.R 
R CMD BATCH --no-save glchart.R 

module load git
git add data/nhldesign.rda results/*effects.txt
git commit -a -m "glfit update"
git push
