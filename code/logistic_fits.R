source("args.R")

load("../data/nhlscrapr_logit_data.RData")
library(slam)
library(textir)
library(mvtnorm)
library(Matrix)
library(reglogit)

## team+player model with special teams indicators
penSTP <- c(rep(0,ncol(XS)+ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSTP <- mnlm(counts=Y, covars=cbind(XS,XT,XP), verb=1, penalty=penSTP, normalize=FALSE)

## player-only model with special teams indicators
penSP <- c(rep(0,ncol(XS)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSP <- mnlm(counts=Y, covars=cbind(XS,XP), verb=1, penalty=penSP, normalize=FALSE)

## save the output, does not include the inputs
save(fitSP, fitSTP, file="../results/logistic_map_fits.RData")

## for parallel reglogits
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
S <- 1000# 0

## do the two reglogits in parallel
cls <- makeCluster(rep("localhost", 2), type="PSOCK")
outs <- reglogit.snow2(cls, S, Y, XS, XT, XP)
stopCluster(cls)

## fully Bayesian team+player model with special teams indicators
S <- 10000
## fitSTP.rl <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
fitSTP.rl <- outs[[1]]
## save the output
## save(fitSTP.rl, file="../results/logistic_rl_fits.RData")

## player-only model
## fitSP.rl <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
fitSP.rl <- outs[[2]]
## save the output
save(fitSP.rl, fitSTP.rl, file="../results/logistic_rl_fits.RData")
