source("args.R")

load("../data/nhlscrapr_logit_data.RData")
library(textir, lib=LIB)

penP <- c(0, rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitP <- mnlm(counts=Y, covars=XP, verb=1, penalty=penP, normalize=FALSE)
sum(coef(fitP)[-1]!=0)

penSP <- c(rep(0,ncol(XS)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSP <- mnlm(counts=Y, covars=cbind(XS,XP), verb=1, penalty=penSP, normalize=FALSE)
sum(coef(fitSP)[-(1:8)]!=0)

penTP <- c(rep(0,ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitTP <- mnlm(counts=Y, covars=cbind(XT,XP), verb=1, penalty=penTP, normalize=FALSE)
sum(coef(fitTP)[-(1:32)]!=0)

penSTP <- c(rep(0,ncol(XS)+ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSTP <- mnlm(counts=Y, covars=cbind(XS, XT,XP), verb=1, penalty=penSTP, normalize=FALSE)
sum(coef(fitSTP)[-(1:39)]!=0)

## save the output, includes the inputs
save.image("../data/nhlscrapr_logit_fits.RData")