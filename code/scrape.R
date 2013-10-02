##### Code to scrape from nhl.com
##### Relies on A.C. Thomas's nhlcrapr package

source("args.R")

## does not write over previous scrapes
gamepath <- paste(EXT, "/nhlgames", sep="")
system(sprintf("mkdir -p %s", gamepath))

## you need to have installed...
library(bitops)
library(RCurl)
library(rjson)
library(nhlscrapr)
library(doMC)
sessionInfo()

## start cluster
registerDoMC(NC)

full.games.database <- function()
{
    game.roster <- NULL
    seasons <- c("20022003", "20032004", "20052006", "20062007", 
        "20072008", "20082009", "20092010", "20102011", "20112012", 
        "20122013", "20132014")
    games <- c(rep(1230, 9), 720, 1230)
    bad.game.list <- list(c(1:127, 134, 135, 582, 598, 872), 
        c(10, 251, 453, 456, 482, 802), c(18, 140, 298, 458, 
            974), c(1024), c(), c(259, 409, 1077), c(81, 827, 
            836, 857, 863, 874), c(429), c(259), c(), c())
    bad.playoff <- matrix(c("20032004", "0134", "20052006", "0233"), 
        nrow = 2)
    game.rec <- array(NA, c(7 * 15, 3))
    count <- 0
    for (kk in 1:4) for (ll in 1:2^(4 - kk)) for (mm in 1:7) {
        count <- count + 1
        game.rec[count, ] <- c(kk, ll, mm)
    }
    gnum <- paste0("0", game.rec[, 1], game.rec[, 2], game.rec[, 
        3])
    for (ss in 1:length(seasons)) {
        gn1 <- as.character(1:games[ss])
        while (any(nchar(gn1) < 4)) gn1[nchar(gn1) < 4] <- paste0("0", 
            gn1[nchar(gn1) < 4])
        df1 <- data.frame(season = seasons[ss], session = c(rep("Regular", 
            games[ss]), rep("Playoffs", length(gnum))), gamenumber = c(gn1, 
            gnum), awayteam = "", hometeam = "", awayscore = "", 
            homescore = "", date = "", valid = c(!(1:games[ss] %in% 
                bad.game.list[[ss]]), rep(TRUE, length(gnum))), 
            stringsAsFactors = FALSE)
        game.roster <- rbind(game.roster, df1)
    }
    game.roster[, 1] <- as.character(game.roster[, 1])
    game.roster[, 2] <- as.character(game.roster[, 2])
    for (kk in 1:dim(bad.playoff)[2]) game.roster$valid[game.roster$season == 
        bad.playoff[1, kk] & game.roster$session == "Playoffs" & 
        game.roster$gamenumber == bad.playoff[2, kk]] <- FALSE
    game.roster$gcode <- paste0(2 + 1 * (game.roster$session == 
        "Playoffs"), game.roster$gamenumber)
    return(game.roster)
}

## grab the full game data
allgames <- full.game.database()
games <- allgames

grcode <- apply(games[,c("season","gcode")], 1, function(r) paste(r,collapse="."))

## subset for only valid ones we don't have
grexist <- sub("-",".", gsub(sprintf("%s|/|-gamerec.txt",gamepath),"",
 		Sys.glob(sprintf("%s/*-gamerec.txt",gamepath))))
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
	    	tryCatch(item <- process.single.game(G$season[i],G$gcode[i],gamepath,save.to.file=TRUE),
	    		  	error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e))) 
	    	if (i%%100 == 0) message(paste("proc game", i, "of chunk", k))
		}
	} 
warnings()
}

## build out roster material and save (note we use allgames here)
roster <- construct.rosters(allgames[allgames$valid,], rdata.folder = gamepath)
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
			g <- retrieve.game(G$season[i], G$gcode[i], gamepath)
			rec <- augment.game(g,master)
			write.table(rec, file=sprintf("%s/%s-%s-gamerec.txt",
					gamepath,G$season[i], G$gcode[i]), 
					sep="|", quote=FALSE) },
			error = function(e) print(paste("CAUGHT at",G[i,1],G$gcode[i],":",e)))
		if (i%%100 == 0) message(paste("write game", i, "of chunk ", k))}
}
warnings()

print(date())
