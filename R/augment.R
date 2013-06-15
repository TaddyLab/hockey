##### Code to augment the dowloaded gams
##### Relies on A.C. Thomas's nhlcrapr package

source("args.R")

## you need to have installed...
library(bitops, lib.loc=LIB)
library(RCurl, lib.loc=LIB)
library(doMC, lib.loc=LIB)
library(nhlscrapr, lib.loc=LIB)

games <- read.table(file="../data/games.txt", 
 				sep="|", comment="", quote="")

## build out roster material
roster.main <- construct.rosters(games, rdata.folder = EXT)
roster <- roster.main$master.list
roster.unique <- roster.main$unique.list
game.table <- roster.main$game.table

## write output to file
write.table(game.table, file="../data/game.table.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)
write.table(roster, file="../data/roster.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)
write.table(roster.unique, file="../data/roster.unique.txt", 
 	sep="|", row.names=FALSE, quote=FALSE)

## augment the game information
registerDoMC(NC)
chunk <- ceiling( (0:NC)*(nrow(game.table)/NC) )
print(chunk)
mcaug <- foreach (k=1:NC) %dopar% {
	G <- game.table[(chunk[k]+1):chunk[k+1],]
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

print(date())

