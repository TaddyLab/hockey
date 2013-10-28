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

source("args.R")

## googlechart:
##
## create a google chart with the specified data, header, footer,
## and output file

googlechart <- function(M, header, footer, outfile)
 {

 	footer <- gsub("5016", as.character(24*nrow(M)), footer)

 	unlink(outfile)

 	cat(header, sep="\n", file=outfile)

 	cn <- colnames(M)
	cat("\t[\'", cn[1], "\', \'", paste(cn[-1], collapse="\',\'"), "\'],\n", 
        	file=outfile, sep="", append=TRUE)

	M[,1] <- sub("'", "", M[,1])

	for(i in 1:nrow(M)) {
        cat("\t[\'", M[i,1], "\', ", paste(M[i,-1], collapse=","), "],\n", 
        	file=outfile, sep="", append=TRUE)
	}

	cat(footer, sep = "\n", file=outfile, append=TRUE)
}

## extract the map betas residing in Git version control
map.betas.all <- read.csv("~/hockey-git/results/logistic_map_betas.csv")
map.betas.all$Player <- as.character(map.betas.all$Player)

## get just current year
map.betas <- map.betas.all[map.betas.all[,2] == 20132014,]

## append last active year info
map.betas.all$Player <- paste(map.betas.all$Player, " (", map.betas.all$Last.Active.Year, ")", sep="")

## add ranks
map.betas$Player <- paste(map.betas$Player, 1:nrow(map.betas), sep=" - ")
map.betas.all$Player <- paste(map.betas.all$Player, 1:nrow(map.betas.all), sep=" - ")

## get non-zero rows
rows <- which(apply(map.betas[,3:4], 1, function(x) { all(x != 0) }))
map.betas.nz <- map.betas[rows,-2]
rows <- which(apply(map.betas.all[,3:4], 1, function(x) { all(x != 0) }))
map.betas.nz.all <- map.betas.all[rows,-2]

## write out all player stats
resultpath <- paste(EXT, "/results_20132014", sep="")
system(sprintf("mkdir -p %s", resultpath))

## read in header and footer files
header <- readLines("~/hockey-git/code/header.html")
footer <- readLines("~/hockey-git/code/footer.html")

## file names
date <- format(Sys.time(), "%Y%m%d")
mapallfile <- paste("mapbetas_all_", date, ".html", sep="")
fullmapallfile <- paste(resultpath, "/", mapallfile, sep="")
mapfilecur <- paste("mapbetas_active_", date, ".html", sep="")
fullmapfilecur <- paste(resultpath, "/", mapfilecur, sep="")

## changes to footer for all
footer.all <- gsub("ability", "ability (since 2001)", footer)
footer.all[39] <- paste("<a href=\"", mapfilecur, "\"> show only current players</a>", sep="")
footer <- gsub("ability", "ability (active)", footer)
footer[39] <- paste("<a href=\"", mapallfile, "\">show all players</a>", sep="")
footer[38] <- footer.all[38] <- paste(Sys.time(), "<br>", sep="")

## write out all
googlechart(map.betas.nz.all, header, footer.all, fullmapallfile)
system(paste("cp -f ", paste(fullmapallfile, " ", resultpath, "/mapbetas_all_latest.html", sep="")))
## write out just those active in 20132014
googlechart(map.betas.nz, header, footer, fullmapfilecur)
system(paste("cp -f ", paste(fullmapfilecur, " ", resultpath, "/mapbetas_active_latest.html", sep="")))

## write out an index
index <- readLines("~/hockey-git/code/index.html")
active <- rev(list.files(resultpath, "active"))[-1]
active <- paste("<a href=\"", active, "\">", active, "</a>", sep="")
active <- paste("<tr><td>", active, "</td>", sep="")
all <- rev(list.files(resultpath, "all"))[-1]
all <- paste("<a href=\"", all, "\">", all, "</a>", sep="")
all <- paste("<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<td>", all, "</td></tr>\n", sep="")
both <- paste(as.vector(rbind(active, all)), collapse="\n")
index[39] <- paste("<table align=\"center\", cellpadding=\"5\">", both, "</table>", sep="")
cat(index, file=paste(resultpath, "index.html", sep="/"))
