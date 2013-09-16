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
map.betas <- read.csv("~/hockey-git/results/logistic_map_betas.csv")
map.betas$Player <- as.character(map.betas$Player)

## get non-zero rows
rows <- which(apply(map.betas[,2:4], 1, function(x) { all(x != 0) }))
map.betas.nz <- map.betas[rows,]

## append last active year info
map.betas.nz.all <- map.betas.nz[,-2]
map.betas.nz.all$Player <- paste(map.betas.nz$Player, map.betas.nz$Last.Active.Year, sep=" : ")

## write out all player stats
resultpath <- paste(EXT, "/results_20122013", sep="")
system(sprintf("mkdir -p %s", gamepath))

mapallfile <- paste(resultpath, "/mapbetas_all_", 
	format(Sys.time(), "%Y%m%d"), ".html", sep="")
googlechart(map.betas.nz.all, "~/hockey-git/code/header.html", 
	"~/hockey-git/code/footer.html", mapallfile)

## write out just those active in 20122013
mapfilecur <- paste(resultpath, "/mapbetas_active_", 
	format(Sys.time(), "%Y%m%d"), ".html", sep="")
googlechart(map.betas.nz[map.betas.nz[,2] == 20122013,-2], 
	"~/hockey-git/code/header.html", "~/hockey-git/code/footer.html", mapfilecur)
