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


## fitting reglogit hockey model
library(mvtnorm)
library(Matrix)
library(reglogit)

## load the data
load("data/nhldesign.rda")

## design matrices
X <- cBind(XS,XT,XP)

## fit the reglogit model
S <- 100#00
fitSTP.rl <- reglogit(S, Y, X, save.latents=FALSE, normalize=FALSE)

## save full MCMC output
save(fitSTP.rl, file="results/rlfits.RData")

## not sure about the rest of this stuff

## calculate posterior means for the STP model
burnin <- 10#00
betaSTP <- fitSTP.rl$beta[-(1:burnin),-(1:(1+ncol(XT)+ncol(XS)))] 
btab <- data.frame(who=colnames(XP), last_active=player$active, 
	mean_effect=apply(betaSTP, 2, mean))

## order and write out to file
bo <- order(btab[,3], decreasing=TRUE)
boutfile <- "results/rl_mean_effects.csv"
write.csv(btab[bo,], file=boutfile, row.names=FALSE, quote=FALSE)

## posterior ranks with teams
ranksSTP <- matrix(NA, ncol=ncol(betaSTP), nrow=nrow(betaSTP))
for(i in 1:nrow(ranksSTP)) ranksSTP[i,] <- rank(-betaSTP[i,])
## now summarize the rank distribution
rna <- rep(NA, ncol(ranksSTP))
rsumm <- data.frame(who=colnames(XP), last_active=player$active, 
				    TPq10=rna, TPmed=rna, TPq90=rna)
for(i in 1:ncol(ranksSTP)) {
	rsumm$TPq10[i] <- quantile(ranksSTP[,i], 0.1)
	rsumm$TPmed[i] <- median(ranksSTP[,i])
	rsumm$TPq90[i] <- quantile(ranksSTP[,i], 0.9)
}

## order by median and write out to file
or <- order(rsumm$TPmed, rsumm$TPq90 - rsumm$TPq10)
routfile <- "results/rl_prank_effects.csv"
write.csv(rsumm[or,], file=routfile, row.names=FALSE, quote=FALSE)