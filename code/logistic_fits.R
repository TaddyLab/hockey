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
fitSTP.rl <- reglogit(S, Y, cbind(XS, XT, XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
## save the output
save(fitSP.rl, file="../results/logistic_rl_fits.RData")


fitSP.rl <- reglogit(S, Y, cbind(XS,XP), save.latents=FALSE, normalize=FALSE, sparse=TRUE)
## save the output
save(fitSP.rl, fitSTP.rl, file="../results/logistic_rl_fits.RData")

## calculate posterior means for the STP model
burnin <- 1000
betaSTP <- fitSTP.rl$beta[-(1:burnin),-(1:39)] 
btab <- data.frame(uN2, active, apply(betaSTP, 2, mean))

# posterior means without teams
betaSP <- fitSP.rl$beta[-(1:burnin),-(1:8)]
btab <- cbind(btab, apply(betaSP, 2, mean))
names(btab) <- c("Player", "Last Active Year", "Player-Team Model", "Player-Only Model")
btab$Player <- as.character(btab$Player)

## order and write out to file
bo <- order(btab[,3], btab[,4], decreasing=TRUE)
boutfile <- paste("../results/logistic_mean_betas_", format(Sys.time(), "%Y%m%d"), ".csv", sep="")
write.csv(btab[bo,], file=boutfile, row.names=FALSE, quote=FALSE)

## posterior ranks with teams
ranksSTP <- matrix(NA, ncol=ncol(betaSTP), nrow=nrow(betaSTP))
for(i in 1:nrow(ranksSTP)) ranksSTP[i,] <- rank(-betaSTP[i,])
ranksSP <- matrix(NA, ncol=ncol(betaSP), nrow=nrow(betaSP))
for(i in 1:nrow(ranksSP)) ranksSP[i,] <- rank(-betaSP[i,])
## now summarize the rank distribution
rna <- rep(NA, ncol(ranksSTP))
rsumm <- data.frame(Player=uN2, active=active, 
				    TPq10=rna, TPmed=rna, TPq90=rna,
					Pq10=rna, Pmed=rna, Pq90=rna)
for(i in 1:ncol(ranksSTP)) {
	rsumm$TPq10[i] <- quantile(ranksSTP[,i], 0.1)
	rsumm$TPmed[i] <- median(ranksSTP[,i])
	rsumm$TPq90[i] <- quantile(ranksSTP[,i], 0.9)
	rsumm$Pq10[i] <- quantile(ranksSP[,i], 0.1)
	rsumm$Pmed[i] <- median(ranksSP[,i])
	rsumm$Pq90[i] <- quantile(ranksSP[,i], 0.9)
}

## order by median and write out to file
or <- order(rsumm$TPmed, rsumm$Pmed, rsumm$TPq2 - rsumm$TPq1)
routfile <- paste("../results/logistic_pranks_betas_", format(Sys.time(), "%Y%m%d"), ".csv", sep="")
write.csv(rsumm[or,], file=boutfile, row.names=FALSE, quote=FALSE)
