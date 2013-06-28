source("args.R")

load("../data/nhlscrapr_logit_data.RData")
library(textir, lib=LIB)
library(reglogit, lib=LIB)

## team+player model with special teams indicators
penSTP <- c(rep(0,ncol(XS)+ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSTP <- mnlm(counts=Y, covars=cbind(XS,XT,XP), verb=1, penalty=penSTP, normalize=FALSE)

## player-only model with special teams indicators
penSP <- c(rep(0,ncol(XS)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSP <- mnlm(counts=Y, covars=cbind(XS,XP), verb=1, penalty=penSP, normalize=FALSE)

## fully Bayesian team+player model with special teams indicators
S <- 10000
fitSP.rl <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
fitSTP.rl <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, normalize=FALSE)

## save the output, does not include the inputs
save(fitSP, fitSTP, fitSP.rl, fitSTP.rl, "../results/logit_fits.RData")
