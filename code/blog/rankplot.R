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


rs <- read.csv("../results/logistic_pranks_betas.csv")

n <- nrow(rs)

prankfile <- paste("../results/prank_all_", 
	format(Sys.time(), "%Y%m%d"), ".pdf", sep="")
pdf(prankfile, height=200)
xrange <- range(rs[,-(1:3)])
par(mai=c(0,2,0.0,0.5), mgp=c(-1,0,0))
plot(rs$TPmed, ((n:1)+0.1), axes=FALSE, col="red", 
	 ylab="", cex=0.5, xlim=xrange, xlab="player rank")
segments(rs$TPq10, ((n:1)+0.1), rs$TPq90, ((n:1)+0.1), col="red")
points(rs$Pmed, ((n:1)-0.1), col="blue", cex=0.5)
segments(rs$Pq10, ((n:1)-0.1), rs$Pq90, ((n:1)-0.1), col="blue")
axis(2, at=(n:1), labels=rs$Player, tick=FALSE, las=2, cex.axis=0.5)
dev.off()
