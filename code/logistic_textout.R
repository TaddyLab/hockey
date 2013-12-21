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


## check that libraries are loaded
library(gamlr)

## load the goals data
load("../data/nhlscrapr_logit_data.RData")
XP <- as(XP,"dgCMatrix")

## MAP

## load the map outputs
load(file="../results/logistic_map_fits.RData")

## create beta table
active <- XG[,'Season'][XP@i[tail(XP@p,-1)] + 1]
who <- data.frame(uN2, active) 
BSP <- coef(fitSP, k=2)[-c(1,fitSP$free+1),]
BTSP <- coef(fitTSP, k=2)[-c(1,fitTSP$free+1),]
## output in player table
tab <- cbind(who,BTSP,BSP)[order(-BTSP,-BSP),]
names(tab) <- c("Player", "Last Active Year", 
	"Player-Team Model", "Player-Only Model")
tab$Player <- as.character(tab$Player)

## write table out to files
outfile <- "../results/logistic_map_betas.csv"
write.csv(tab, file=outfile, row.names=FALSE, quote=FALSE)

## full Bayes

## load the fully Bayesian outputs
load(file="../results/logistic_rl_fits.RData")

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
boutfile <- "../results/logistic_mean_betas.csv"
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
or <- order(rsumm$TPmed, rsumm$Pmed, rsumm$TPq90 - rsumm$TPq10)
routfile <- "../results/logistic_pranks_betas.csv"
write.csv(rsumm[or,], file=routfile, row.names=FALSE, quote=FALSE)
