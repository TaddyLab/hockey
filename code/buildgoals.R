source("args.R")

## load the roster object containing player/game info
load("../data/roster.RData")

game.goals <- function(file, session)
  {
  	## make sure the file exists
  	if(!file.exists(file)) return(NULL)

	## first read game file
	game <- read.csv(file, sep="|", comment="", quote="")

	## get goals
	goals <- game[which(game$etype == "GOAL"),]

	## remove <= period 4 for regular season games
	if(session == "Regular") {
		goals <- goals[goals$period <= 4,]
		if(nrow(goals) == 0) return(NULL)
	}

	## check to make sure there are skaters on the ice
	## (i.e., not shootout or penalty shot)
	shootout <- goals[,c(8:12,14:18)] != 1 
	w <- which(apply(shootout, 1, function(x) { sum(x) > 6 }))
	eg <- goals[w,]

	## remove un-needed columns
	eg <- eg[,-c(3,7,21:28,33)]

	## replace ev.team with home or away goal scoring team
	eg$ev.team <- as.character(eg$ev.team)
	eg$awayteam <- as.character(eg$awayteam)
	eg$hometeam <- as.character(eg$hometeam)
	g <- rep(NA, nrow(eg))
	g[eg$ev.team == eg$awayteam] <- "AWAY"
	g[is.na(g)] <- "HOME"
	session <- rep(session, nrow(eg)) 
	eg <- cbind(eg, g, session) 
	eg$ev.team <- NULL

	## return the result
	return(eg)
}
  
## extract games, and prefix for location of files
G <- roster$games

## empty games data structure
games <- list()

## loop over games
for(i in 1:nrow(G)) {

	## next if not valid or regular season
	if(! (G[i,]$valid)) { games[[i]] <- NULL; next }

	## construct the filke
	file <- sprintf("%s/%s-%s-gamerec.txt", EXT, G$season[i], G$gcode[i])

	## get the game
	games[[i]] <- game.goals(file, G[i,]$session)

	## progress meter
	if(i %% 100 == 0) 
		cat(i, " of ", nrow(G), " games processed\n", sep="")
}

## collapse list of data frames into one data frames
goals <- do.call("rbind", games)

## deal with types of columsn
goals$g <- as.factor(goals$g)
goals$hometeam <- as.factor(goals$hometeam)
goals$awayteam <- as.factor(goals$awayteam)

## save to file
save(goals, file="../data/nhlscrapr_goals.RData")

## now build design matrix and response -- 
## this comes from the newdata.R script

## goal indicator(s)
Y <- goals$g == "HOME"

## home and away team indicators
TA <- as.numeric(goals$awayteam)
TH <- as.numeric(goals$hometeam)

## create unique names --
## this is overkill given the new roster structure
uID <- 1:nrow(roster$roster.unique)
uN <- paste(roster$roster.unique$first, roster$roster.unique$last, sep="_")
positions <- uniqueNames <- rep(NA, length(unique(uID)))
uniqueNames[uID] <- uN
positions[uID] <- roster$roster.unique$pos

## home and away player indicators
PA <- as.matrix(goals[,c(6:11,22)])
PA[PA == 1] <- NA
PH <- as.matrix(goals[,c(12:17,23)])
PH[PH == 1] <- NA

## build design matrix and make Plus-minus comparison
nplayers <- nrow(roster$roster.unique)
T <- nrow(PH)
## players the design matrix
XP <- matrix(0, ncol=nplayers, nrow=T)
colnames(XP) <- paste("P", 1:nplayers, sep="")

## teams design matrix
nteams <- max(TH)
XT <- matrix(0, ncol=nteams, nrow=T)
colnames(XT) <- paste("T", 1:nteams, sep="")
## plus-minus calculation
Ppm <- rep(0, ncol(XP)) ## players
Tpm <- rep(0, ncol(XT))

## special teams design matrix
XS <- matrix(0, ncol=7, nrow=T)
colnames(XS) <- c("S6v5", "S6v4", "S6v3", "S5v4", "S5v3", "S4v3", "SNG")
XS <- as.data.frame(XS)

## build the matrices and plus-minus
for(t in 1:T) {

  ## design matrix for teams and players	
  pht <- PH[t,!is.na(PH[t,])]
  pat <- PA[t,!is.na(PA[t,])]
  XT[t, TH[t]] <- XP[t, pht] <- 1
  XT[t, TA[t]] <- XP[t, pat] <- -1

  ## design matrix for special teams
  spht <- sum(!is.na(PH[t,1:6]))
  spat <- sum(!is.na(PA[t,1:6]))
  if(spht == 6) {
  	if(spat == 5) XS[t,]$S6v5 <- 1
  	else if(spat == 4) XS[t,]$S6v4 <- 1
  	else if(spat == 3) XS[t,]$S6v3 <- 1
  } else if(spht == 5) {
  	if(spat == 6) XS[t,]$S6v5 <- -1
    else if(spat == 4) XS[t,]$S5v4 <- 1
    else if(spat == 3) XS[t,]$S5v3 <- 1
  } else if(spht == 4) {
  	if(spat == 3) XS[t,]$S4v3 <- 1
  	else if(spat == 5) XS[t,]$S5v4 <- -1
  	else if(spat == 6) XS[t,]$S6v4 <- -1
  } else if(spht == 3) {
  	if(spat == 4) XS[t,]$S4v3 <- -1
  	else if(spat == 5) XS[t,]$S5v3 <- -1
  	else if(spat == 6) XS[t,]$S6v3 <- -1
  }

  ## checking for pulled-goalie situation at the end of the game
  if(goals[t,]$session == "Regular" 
  	&& goals[t,]$period == 3 && goals[t,]$seconds > 3480) {
    	if(is.na(PH[t,7]) && (goals[t,]$home.score < goals[t,]$away.score)) XS[t,]$SNG <- 1
    	if(is.na(PA[t,7]) && (goals[t,]$home.score > goals[t,]$away.score)) XS[t,]$SNG <- -1
  }

  ## plus-minus
  if(Y[t]) { ## home goal
    Ppm[pht] <- Ppm[pht] + 1
    Ppm[pat] <- Ppm[pat] - 1
    Tpm[TH[t]] <- Tpm[TH[t]] + 1
    Tpm[TA[t]] <- Tpm[TA[t]] - 1
  } else {
    Ppm[pht] <- Ppm[pht] - 1
    Ppm[pat] <- Ppm[pat] + 1
    Tpm[TH[t]] <- Tpm[TH[t]] - 1
    Tpm[TA[t]] <- Tpm[TA[t]] + 1
  }

  ## progress meter
  if(t %% 1000 == 0) 
	cat(t, " of ", T, " goals processed\n", sep="")
}

## back to matrix
XS <- as.matrix(XS)

## see which cols are all the same, and remove from df
same <- which(apply(XP, 2, var) == 0)
XP <- XP[,-same]
uN2 <- uniqueNames[-same]
pos2 <- positions[-same]
Ppm <- Ppm[-same]

## sanity check
if(any(apply(XT, 2, var) == 0)) error("some teams never play?")

## design matrix containing game info
XG <- matrix(0, ncol=6, nrow=T)
colnames(XG) <- c("Hscore", "Ascore", "Session", "Season", "GameCode", "Seconds")
XG[,1] <- goals$home.score
XG[,2] <- goals$away.score
XG[,3] <- goals$session
XG[,4] <- goals$season
XG[,5] <- goals$gcode
XG[,6] <- goals$seconds

## build data frame
save(uN2, goals, Y, XP, XT, Ppm, Tpm, pos2,
     XS, XG, file="../data/nhlscrapr_logit_data.RData")

## extra cleaning and output for gamlr
library(Matrix)
player <- Matrix(XP,sparse=TRUE)
colnames(player) <- uN2
onice <- Matrix(XS,sparse=TRUE)
goal <- data.frame(whoscored=goals$g,
      season=goals$season, 
      team.away=goals$awayteam,
      team.home=goals$hometeam,
      period=goals$period,
      differential=goals$home.score-goals$away.score,
      session=goals$session,
      gamecode=goals$gcode)
rownames(player) <- rownames(goal) <- rownames(onice) <- 1:nrow(player)
save(goal,player,onice,compress="xz",file="../data/hockey.rda")
