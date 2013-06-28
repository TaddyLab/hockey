
googlechart <- function(M, headfile, footfile, outfile)
 {
 	header <- readLines(headfile)
 	footer <- readLines(footfile)

 	unlink(outfile)

 	cat(header, sep="\n", file=outfile)

 	cn <- colnames(M)
	cat("\t[\'", cn[1], "\', \'", paste(cn[-1], collapse="\',\'"), "\'],\n", 
        	file=outfile, sep="", append=TRUE)

	for(i in 1:nrow(M)) {
        cat("\t[\'", M[i,1], "\', ", paste(M[i,-1], collapse=","), "],\n", 
        	file=outfile, sep="", append=TRUE)
	}

	cat(footer, sep = "\n", file=outfile, append=TRUE)
}

M <- cbind(rownames(BS2), BS2)[nrow(BS2):1,]
colnames(M) <- c("Player", "Player Model", "Player-Team Model")


googlechart(M, "~/hockey-git/code/header.html", "~/hockey-git/code/footer.html",
			"~/hockey-git/code/googlechart.html")