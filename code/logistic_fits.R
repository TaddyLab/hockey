source("args.R")

load("../data/nhlscrapr_logit_data.RData")
library(slam, lib=LIB)
library(textir, lib=LIB)
library(mvtnorm, lib=LIB)
library(Matrix, lib=LIB)
library(reglogit, lib=LIB)
## library(gputools, lib=LIB)

## team+player model with special teams indicators
penSTP <- c(rep(0,ncol(XS)+ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSTP <- mnlm(counts=Y, covars=cbind(XS,XT,XP), verb=1, penalty=penSTP, normalize=FALSE)

## create beta table
XG <- as.data.frame(XG)
active <- rep(NA, ncol(XP))
for(i in 1:ncol(XP)) active[i] <- max(XG$Season[XP[,i] != 0])
tab <- data.frame(uN2, active, matrix(fitSTP$loadings[,-(1:38)], ncol=1))

## player-only model with special teams indicators
penSP <- c(rep(0,ncol(XS)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSP <- mnlm(counts=Y, covars=cbind(XS,XP), verb=1, penalty=penSP, normalize=FALSE)

## augment beta table
tab <- cbind(tab, matrix(fitSP$loadings[,-(1:7)], ncol=1))
names(tab) <- c("Player", "Last Active Year", "Player-Team Model", "Player-Only Model")
tab$Player <- as.character(tab$Player)

## order rows
o <- order(tab[,3], tab[,4], decreasing=TRUE)

## save the output, does not include the inputs
save(fitSP, fitSTP, file="../results/logistic_map_fits.RData")

## write table out to files\
outfile <- paste("../results/logistic_map_betas_", format(Sys.time(), "%Y%m%d"), ".csv", sep="")
write.csv(tab[o,], file=outfile, row.names=FALSE, quote=FALSE)

## fully Bayesian team+player model with special teams indicators
S <- 10000
fitSP.rl <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
save(fitSP.rl, file="../results/logistic_rl_fits.RData")

fitSTP.rl <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
save(fitSP.rl, fitSTP.rl, file="../results/logistic_rl_fits.RData")