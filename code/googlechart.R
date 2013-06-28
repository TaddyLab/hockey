
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

map.betas <- read.csv("~/hockey-git/results/logistic_map_betas_20130628.csv")
map.betas$Player <- as.character(map.betas$Player)

## get non-zero rows
rows <- which(apply(map.betas[,2:4], 1, function(x) { all(x != 0) }))
map.betas.nz <- map.betas[rows,]

map.betas.nz.all <- map.betas.nz[,-2]
map.betas.nz.all$Player <- paste(map.betas.nz$Player, map.betas.nz$Last.Active.Year, sep=" : ")

googlechart(map.betas.nz.all, 
			"~/hockey-git/code/header.html", 
			"~/hockey-git/code/footer.html",
			"~/hockey-git/results/mapbetas_all.html")

googlechart(map.betas.nz[map.betas.nz[,2] == 20122013,-2], 
			"~/hockey-git/code/header.html", "~/hockey-git/code/footer.html",
			"~/hockey-git/results/mapbetas_20122013.html")