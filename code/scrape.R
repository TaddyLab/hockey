##### Code to scrape from nhl.com
##### Relies on A.C. Thomas's nhlcrapr package

source("args.R")

## does not write over previous scrapes
system(sprintf("mkdir -p %s",EXT))

## you need to have installed...
library(bitops)
library(RCurl)
library(rjson)
library(nhlscrapr)
library(doMC)
sessionInfo()

## start cluster
registerDoMC(NC)

## grab the full game data
allgames <- full.game.database()
games <- allgames

grcode <- apply(games[,c("season","gcode")], 1, function(r) paste(r,collapse="."))

## subset for only valid ones we don't have
grexist <- sub("-",".", gsub(sprintf("%s|/|-gamerec.txt",EXT),"",
 		Sys.glob(sprintf("%s/*-gamerec.txt",EXT))))
if(length(grexist) > 0)
	games <- games[grcode>max(grexist),]

## subset to valid games
games <- games[games$valid,]
print(ng <- nrow(games))

## process games in parallel
if(ng > 0) {
	NCP = NC
	if(NCP>ng) NCP = ng
	chunk <- ceiling( (0:NCP)*(nrow(games)/NCP) )
	print(chunk)
	mcproc <- foreach (k=1:NCP) %dopar% {
		G <- games[(chunk[k]+1):chunk[k+1],]
		for(i in 1:nrow(G)){
	    	tryCatch(item <- process.single.game(G$season[i],G$gcode[i],EXT,save.to.file=TRUE),
	    		  	error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e))) 
	    	if (i%%100 == 0) message(paste("proc game", i, "of chunk", k))
		}
	} 
warnings()
}

## build out roster material and save (note we use allgames here)
roster <- construct.rosters(allgames[allgames$valid,], rdata.folder = EXT)
save(roster, file="../data/roster.RData")

## extract valid games
validgames <- roster$games[roster$games$valid,]
master <- roster$roster.master

## augment the game information in parallel
chunk <- ceiling( (0:NC)*(nrow(validgames)/NC) )
print(chunk)
mcaug <- foreach (k=1:NC) %dopar% {
	G <- validgames[(chunk[k]+1):chunk[k+1],]
	for(i in 1:nrow(G)){
		tryCatch({
			g <- retrieve.game(G$season[i], G$gcode[i], EXT)
			rec <- augment.game(g,master)
			write.table(rec, file=sprintf("%s/%s-%s-gamerec.txt",
					EXT,G$season[i], G$gcode[i]), 
					sep="|", quote=FALSE) },
			error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e)))
		if (i%%100 == 0) message(paste("write game", i, "of chunk ", k))}
}
warnings()

print(date())


