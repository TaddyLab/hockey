source("args.R")

## googlechart:
##
## create a google chart with the specified data, header, footer,
## and output file

googlechart <- function(M, headfile, footfile, outfile)
 {
 	header <- readLines(headfile)
 	footer <- readLines(footfile)
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

mapallfile <- paste(resultpath, "/mapbetas_all_", 
	format(Sys.time(), "%Y%m%d"), ".html", sep="")
googlechart(map.betas.nz.all, "~/hockey-git/code/header.html", 
	"~/hockey-git/code/footer.html", mapallfile)

## write out just those active in 20132014
mapfilecur <- paste(resultpath, "/mapbetas_active_", 
	format(Sys.time(), "%Y%m%d"), ".html", sep="")
googlechart(map.betas.nz, 
	"~/hockey-git/code/header.html", "~/hockey-git/code/footer.html", mapfilecur)
