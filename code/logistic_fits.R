source("args.R")

load("../data/nhlscrapr_logit_data.RData")
library(textir, lib=LIB)

penSTP <- c(rep(0,ncol(XS)+ncol(XT)+1), rep(data.frame(c(7.5,1/2)), ncol(XP)))
fitSTP <- mnlm(counts=Y, covars=cbind(XS, XT,XP), verb=1, penalty=penSTP, normalize=FALSE)
sum(coef(fitSTP)[-(1:39)]!=0)

## save the output, includes the inputs
save.image("../results/nhlscrapr_logit_fits.RData")