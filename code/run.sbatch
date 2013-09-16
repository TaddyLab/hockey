#!/bin/bash

#SBATCH --constraint="ib"
#SBATCH --exclusive

module load R/3.0
module load openmpi

## R CMD BATCH --no-save scrape.R  scrape.log
## R CMD BATCH --no-save buildgoals.R buildgoals.log
~/R-3.0.1/bin/R CMD BATCH --no-save logistic_fits.R logistic_fits.log
## R CMD BATCH --no-save logistic_textout.R logistic_textout.log
## R CMD BATCH --no-save googlechart.R googlechart.log

## module load git
## git commit -a -m "nightly update"
