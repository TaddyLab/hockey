##### Code to scrape from nhl.com
##### Relies on A.C. Thomas's nhlcrapr package

source("args.R")

## does not pre-delete previous scrapes
system(sprintf("mkdir -p %s",EXT))

## you need to have installed...
library(bitops, lib.loc=LIB)
library(RCurl, lib.loc=LIB)
library(rjson, lib.loc=LIB)
library(nhlscrapr, lib.loc=LIB)
library(doMC, lib.loc=LIB)

## start cluster
registerDoMC(NC)

allgames <- full.game.database()
## you can subset here to scrape fewer seasons
games <- allgames[as.numeric(allgames$season)>=20022003,]

## process games in parallel
chunk <- ceiling( (0:NC)*(nrow(games)/NC) )
print(chunk)
mcproc <- foreach (k=1:NC) %dopar% {
	G <- games[(chunk[k]+1):chunk[k+1],]
	for(i in 1:nrow(G)){
	    tryCatch(item <- process.single.game(G[i,1],G$gcode[i],EXT,save.to.file=TRUE),
	    		  error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e))) 
	    if (i%%100 == 0) message(paste("proc game", i, "of chunk", k))
	}
} 
warnings()

## build out roster material and save
roster <- construct.rosters(games, rdata.folder = EXT)
save(roster, file="../data/roster.RData")

## extract valid games
validgames <- roster$games[roster$games$valid,]
master <- roster$master.list

## augment the game information in parallel
chunk <- ceiling( (0:NC)*(nrow(validgames)/NC) )
print(chunk)
mcaug <- foreach (k=1:NC) %dopar% {
	G <- validgames[(chunk[k]+1):chunk[k+1],]
	for(i in 1:nrow(G)){
		tryCatch({
			rec <- augment.game(retrieve.game(G$season[i], G$gcode[i], EXT),master)
			write.table(rec, file=sprintf("%s/%s-%s-gamerec.txt",
					EXT,G$season[i], G$gcode[i]), 
					sep="|", quote=FALSE) },
			error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e)))
		if (i%%100 == 0) message(paste("write game", i, "of chunk ", k))}
}
warnings()

print(date())


