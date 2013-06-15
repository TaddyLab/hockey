##### Code to scrape from nhl.com
##### Relies on A.C. Thomas's nhlcrapr package

source("args.R")
START = Sys.time()

## does not pre-delete previous scrapes
system(sprintf("mkdir -p %s",EXT))

## you need to have installed...
library(bitops, lib.loc=LIB)
library(RCurl, lib.loc=LIB)
library(doMC, lib.loc=LIB)
library(nhlscrapr, lib.loc=LIB)

## start cluster
registerDoMC(NC)

allgames <- full.game.database()
## you can subset here to scrape fewer seasons
games <- allgames[as.numeric(allgames$season)==20022003,]
chunk <- ceiling( (0:NC)*(nrow(games)/NC) )

## grab and process games in parallel
mcproc <- foreach (k=1:NC) %dopar% {
	G <- games[(chunk[k]+1):chunk[k+1],]
	for(i in 1:nrow(G)){
	    tryCatch(item <- process.single.game(
    	    		G[i,1],
        			G$gcode[i],
          			rdata.folder = EXT,
          			save.to.file=TRUE),
	    		error = function(e)
	    			message(paste("WARNING in game",
	    						G[i,1],G$gcode[i],":",e)))
	    if (i%%100 == 0) 
	        message(paste("proc game", i, "of chunk", k))
	}
}
warnings()

## build out roster material
roster.main <- construct.rosters(games, rdata.folder = EXT)
roster <- roster.main$master.list
roster.unique <- roster.main$unique.list
games <- roster.main$game.table

## write output to file
write.table(games, file="../data/games.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)
write.table(roster, file="../data/roster.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)
write.table(roster.unique, file="../data/roster.unique.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)

## augment the game information
registerDoMC(NC)
chunk <- ceiling( (0:NC)*(nrow(games)/NC) )
print(chunk)
mcaug <- foreach (k=1:NC) %dopar% {
	G <- games[(chunk[k]+1):chunk[k+1],]
	for(i in 1:nrow(G)){
	      if(G$valid[i])
		tryCatch({pl.table <- open.game(G$season[i], G$gcode[i],EXT)
			  if (length(pl.table$game.record) > 0){ 
			   	rec <- augment.game(pl.table,roster, 
				 		G$season[i], G$gcode[i])
				write.table(rec, file=sprintf("%s/%s-%s-gamerec.txt",
						EXT,G$season[i], G$gcode[i]), 
						sep="|", quote=FALSE) }},
			error = function(e) 
			      message(paste("WARNING in",G$season[i],G$gcode[i],":",e)))  
		if (i%%100 == 0) message(paste("augment: game", i, "of chunk ", k))
}
warnings()

## clean and exit
system(sprintf("rm %s/*.RData", EXT)) 
END <- Sys.time()
print(sprintf("Scrape took %g minutes.",round((END-START)/60,2)))
print(date())


