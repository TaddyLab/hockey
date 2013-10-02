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
map.betas <- map.betas.all[map.betas.all[,2] == 20122013,]

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
resultpath <- paste(EXT, "/results_20122013", sep="")
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
footer.all <- gsub("ability", "ability (all: 2001/02-today)", footer)
footer.all[17] <- paste("show <a href=\"", mapfilecur, "\"> only current players</a>", sep="")
footer <- gsub("ability", "ability (active)", footer)
footer[17] <- paste("show <a href=\"", mapallfile, "\">all players</a>", sep="")
footer[16] <- footer.all[16] <- paste(Sys.time(), "<br>", sep="")

## write out all
googlechart(map.betas.nz.all, header, footer.all, fullmapallfile)
system(paste("ln -sf ", paste(mapallfile, " ", resultpath, "/mapbetas_all_today.html", sep="")))
## write out just those active in 20122013
googlechart(map.betas.nz, header, footer, fullmapfilecur)
system(paste("ln -sf ", paste(mapfilecur, " ", resultpath, "/mapbetas_active_today.html", sep="")))
