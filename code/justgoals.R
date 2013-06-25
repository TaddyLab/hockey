
## code to build a goals matrix
source("args.R")
grecs <- Sys.glob(sprintf("%s/*-gamerec.txt",EXT))

NG <- length(grecs)
cat(readLines(grecs[1],n=1),file="../data/goals.txt",sep="\n")

i <- 0
for(g in grecs){
	rec <- read.table(g, sep="|", 
	                  comment="", quote="",
	                  as.is=TRUE,header=TRUE)

	write.table(rec[rec$etype=="GOAL",], 
	            file="../data/goals.txt", 
	            sep="|", quote=FALSE, 
	            col.names=FALSE, row.names=FALSE, 
	            append=TRUE)
	i <- i+1
	if (i%%1000 == 0) 
        message(paste("goal matrix assembly: game", i, "of", NG))
}
