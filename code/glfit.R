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

## fitting the hockey gl model
library(gamlr)

## grab data
load("data/nhldesign.rda")

## design 
X <- cBind(XS,XT,XP)

### career player effects
fit <- gamlr(X, Y, 
	gamma=10, standardize=FALSE, verb=0,
	family="binomial", free=1:ncol(XS))
B <- coef(fit)[-1,]
Bplayer <- B[colnames(XP)]
cat("nonzero career effects:", sum(Bplayer!=0),"\n")

## pull out team effects
teamtab <- matrix(0,
	nrow=length(teams),ncol=length(seasons),
	dimnames=list(teams,seasons))
bt <- B[colnames(XT)]
bi <- t(matrix(unlist(strsplit(names(bt), "\\.")),nrow=2))
teamtab[bi] <- bt
write.csv(teamtab,"results/gl_team_effects.csv", quote=FALSE, row.names=FALSE)

### current season effects
thisseason <- "20132014"
now <- goal$season==thisseason

## fit ignoring this season
fit_past <- gamlr(X[!now,], Y[!now], 
	free=1:ncol(XS), gamma=10, standardize=FALSE, family="binomial")
Bpast <- coef(fit_past)[colnames(XP),]

## fit any change in current season
fit_now <- gamlr(X[now,], Y[now], 
	fix=predict(fit_past,X[now,]),
	gamma=10, standardize=FALSE, family="binomial")
Bdif <- coef(fit_now)[colnames(XP),]
cat("nonzero current differences:", sum(Bdif!=0),"\n")

## combine
Bcar <- B[colnames(XP)]
Bnow <- (Bdif + Bpast)*(player$active==thisseason)

## tabulate
tab <- data.frame(who=names(Bcar),
			last_active=player$active,
			career_effect=Bcar,
			current_effect=Bnow)
tab <- tab[order(-tab$career_effect,-tab$current_effect,tab$who),]
rownames(tab) <- 1:nrow(tab)
## print(tab[1:25,])

write.csv(tab, file="results/gl_player_effects.csv", row.names=FALSE, quote=FALSE)
