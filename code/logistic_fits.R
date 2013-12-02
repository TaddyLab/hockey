#*******************************************************************************
#
# Chicago Hockey Analytics: Robert B, Gramacy and Matt Taddy
# Copyright (C) 2013, The University of Chicago
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# Questions? Contact Robert B. Gramacy (rbgramacy@chicagobooth.edu), or
#                    Matt Taddy (taddy@chicagobooth.edu)
#
#*******************************************************************************


## load the data
load("../data/nhlscrapr_logit_data.RData")

## load gamlr for MAP analysis
library(gamlr)

## design matrices
XP <- as(XP,"dgCMatrix")
stopifnot(all(colSums(XP!=0)!=0))
XSP <- cBind(XS,XP)
XTSP <- cBind(XT,XSP)

## fit player-only model
fitSP <- gamlr(XSP, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:ncol(XS), verb=1)
## fit player-team model
fitTSP <- gamlr(XTSP, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:(ncol(XS)+ncol(XT)), verb=1)

## save the output, does not include the inputs
save(fitSP, fitTSP, file="../results/logistic_map_fits.RData")


## now for reglogit
library(mvtnorm)
library(Matrix)
library(reglogit)
library(parallel)


## reglogit.chunk:
##
## this is just a switch function calling the player-team fit or the player-only fit
## -- designed to work with reglogit.snow2

reglogit.chunk <- function(chunk, S, Y, XS, XT, XP)
	{
		if(chunk == "team-player") {
			out <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, 
				normalize=FALSE, sparse=TRUE)
		} else if(chunk == "player") {
			out <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, 
				normalize=FALSE, sparse=TRUE)
		} else stop("unknown chunk type")

		return(out)
	}


## reglogit.snow2:
## 
## wrapper funtion that calls a clusterApply on the reglogit.chunk functoin
## in order to parallelize the calculation of the player-team fit and the player-only
## fit
	
reglogit.snow2 <- function(cls, S, Y, XS, XT, XP)
	{
		if(length(cls) != 2) stop("only makes sense to use this function with a 2-cluster")
		chunks <- list("team-player", "player")
		clusterEvalQ(cls, library(mvtnorm))
		clusterEvalQ(cls, library(Matrix))
		clusterEvalQ(cls, library(reglogit))
		outs <- clusterApply(cls, chunks, reglogit.chunk, S=S, Y=Y, XS=XS, XT=XT, XP=XP)
	}

## length of MCMC run
S <- 10000

## do the two reglogits in parallel
cls <- makeCluster(rep("localhost", 2), type="PSOCK")
outs <- reglogit.snow2(cls, S, Y, XS, XT, XP)
stopCluster(cls)

## fully Bayesian team+player model with special teams indicators
## fitSTP.rl <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
fitSTP.rl <- outs[[1]]
## save the output
## save(fitSTP.rl, file="../results/logistic_rl_fits.RData")

## player-only model
## fitSP.rl <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
fitSP.rl <- outs[[2]]
## save the output
save(fitSP.rl, fitSTP.rl, file="../results/logistic_rl_fits.RData")
