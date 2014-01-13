## googlechart:
##
## create a google chart with the specified data, header, footer,
## and output file

URL <- "mtaddy@faculty.chicagobooth.edu:/Faculty/matt.taddy/hockey" 

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

thisseason <- "20132014"

## write out all player stats
resultpath <- "external/results_20132014"
system(sprintf("mkdir -p %s",resultpath))

## read in header and footer files
header <- readLines("code/header.html")
footer <- readLines("code/footer.html")

## file names
date <- format(Sys.time(), "%Y%m%d")
allfile <- paste("mapbetas_all_", date, ".html", sep="")
allpath <- paste(resultpath, "/", allfile, sep="")
curfile <- paste("mapbetas_active_", date, ".html", sep="")
curpath <- paste(resultpath, "/", curfile, sep="")

## changes to footer for all
footer.all <- gsub("ability", 
	'effect: career average / <font color="red">this season</font>', footer)
footer.all[39] <- paste("<a href=\"", curfile, "\"> show only current players</a>", sep="")
footer <- gsub("ability", 
	'effect (active): career average / <font color="red">this season</font>', footer)
footer[39] <- paste("<a href=\"", allfile, "\">show all players</a>", sep="")
footer[38] <- footer.all[38] <- paste(Sys.time(), "<br>", sep="")

## extract the map betas residing in Git version control
tab <- read.table("results/player_effects.txt", 
	sep="|", header=TRUE, quote="", comment="",
	colClasses=c("character","character","numeric","numeric"))

## write out nonzero carreer or current
alltab <- tab[(tab$current_effect!=0)|(tab$career_effect!=0),]
alltab$who <- paste(alltab$who, " (", alltab$last_active, ")", sep="")
alltab <- subset(alltab,select=-last_active)
alltab <- alltab[order(-alltab$career_effect),]
alltab$who <- paste(alltab$who, 1:nrow(alltab), sep=" - ")
googlechart(alltab, header, footer.all, allpath)
system(paste("cp -f ", paste(allpath, " ", resultpath, "/mapbetas_all_latest.html", sep="")))

## write out nonzero current effects
curtab <- tab[tab$current_effect!=0,]
curtab <- subset(curtab,select=-last_active)
curtab <- curtab[order(-curtab$current_effect),]
curtab$who <- paste(curtab$who, 1:nrow(curtab), sep=" - ")
googlechart(curtab, header, footer, curpath)
system(paste("cp -f ", paste(curpath, " ", resultpath, "/mapbetas_active_latest.html", sep="")))

## write out an index
index <- readLines("code/index.html")
active <- rev(list.files(resultpath, "active"))[-1]
active <- paste("<a href=\"", active, "\">", active, "</a>", sep="")
active <- paste("<tr><td>", active, "</td>", sep="")
all <- rev(list.files(resultpath, "all"))[-1]
all <- paste("<a href=\"", all, "\">", all, "</a>", sep="")
all <- paste("<td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<td>", all, "</td></tr>\n", sep="")
both <- paste(as.vector(rbind(active, all)), collapse="\n")
index[39] <- paste("<table align=\"center\", cellpadding=\"5\">", both, "</table>", sep="")
indexfile <- paste(resultpath, "index.html", sep="/")
cat(index, file=indexfile)

## post it all online
system(paste("scp ", paste(allpath, " ", URL, "/mapbetas_all_latest.html", sep="")))
system(paste("scp ", paste(allpath, " ", URL, "/", allfile, sep="")))
system(paste("scp ", paste(curpath, " ", URL, "/mapbetas_active_latest.html", sep="")))
system(paste("scp ", paste(curpath, " ", URL, "/", curfile, sep="")))
system(paste("scp ", paste(indexfile, " ", URL, "/index.html", sep="")))


