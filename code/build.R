## building goal data

library(distrom) ## on CRAN; includes Matrix, gamlr, parallel
NC <- detectCores()
cl <- makeCluster(NC,type="FORK")

game.goals <- function(file, session)
  {
        ## make sure the file exists
        if(!file.exists(file)) return(NULL)

        ## first read game file
        game <- read.csv(file, sep="|", comment="", quote="")

        ## get goals
        goals <- game[which(game$etype == "GOAL"),]

        ## remove <= period 4 for regular season games
        if(session == "Regular")
                goals <- goals[goals$period <= 4,]

        ## check to make sure there were any goals in regulation time
        if(nrow(goals) == 0) return(NULL)
        rownames(goals) <- 1:nrow(goals)

        ## check to make sure there are skaters on the ice
        ## (i.e., not shootout or penalty shot)
        shootout <- goals[,c(8:19,34:35)] != 1 
        w <- which(apply(shootout, 1, function(x) { sum(x) >= 8 }))
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
        session <- rep(session, nrow(eg))  ## also add session variable
        eg <- cbind(eg, g, session) 
        eg$ev.team <- NULL

        ## return the result
        return(eg)
}

season.goals <- function(gmat){
  glist <- list()
  for(i in rownames(gmat)){
      if(gmat[i,]$valid)
        glist[[i]] <- game.goals(
            file=sprintf("externals/%s-gamerec.txt",i), 
            session=gmat[i,]$session)
  print(i)
  }
  do.call(rbind,glist)
}
  
## grab games, break into list, and loop
load("data/roster.RData")
G <- roster$games
rownames(G) <- sprintf("%s-%s", G$season, G$gcode)
G <- lapply(unique(G$season), function(s) G[G$season==s,])
goals <- do.call(rbind,parLapply(cl,G,season.goals))

## deal with types of columns and fix team names
goals$g <- as.factor(goals$g)
goals$awayteam <- as.character(goals$awayteam)
goals$hometeam <- as.character(goals$hometeam)
goals$awayteam[goals$awayteam == "S.J"] <- "SJS"
goals$awayteam[goals$awayteam == "L.A"] <- "LAK"
goals$awayteam[goals$awayteam == "T.B"] <- "TBL"
goals$awayteam[goals$awayteam == "N.J"] <- "NJD"
goals$hometeam[goals$hometeam == "S.J"] <- "SJS"
goals$hometeam[goals$hometeam == "L.A"] <- "LAK"
goals$hometeam[goals$hometeam == "T.B"] <- "TBL"
goals$hometeam[goals$hometeam == "N.J"] <- "NJD"
goals$hometeam <- as.factor(goals$hometeam)
goals$awayteam <- as.factor(goals$awayteam)

## goal indicator(s)
Y <- goals$g == "HOME"
ngoals <- nrow(goals)

## unique player info
nplayers <- nrow(roster$roster.unique)
unames <- paste(roster$roster.unique$first, 
            roster$roster.unique$last, sep="_")
positions <- roster$roster.unique$pos

goals.design <- function(gmat){

  ## Design matrices
  XP <- Matrix(0, nrow=ngoals,ncol=nplayers,
        dimnames=list(rownames(goals),unames))


  ## home and away player indicators
  PA <- as.matrix(goals[,c(6:11,22)])
  PA[PA == 1] <- NA
  PH <- as.matrix(goals[,c(12:17,23)])
  PH[PH == 1] <- NA

## special teams design matrix
XS <- matrix(0, ncol=7, nrow=T)
colnames(XS) <- c("S6v5", "S6v4", "S6v3", "S5v4", "S5v3", "S4v3", "SNG")
XS <- as.data.frame(XS)

## split into chgunks
chunks <- round(seq.int(0,ngoals,length=NC*2+1))
G <- lapply(1:(NC*2), function(i) goals[(chunk[i]+1):chunk[i+1],])



## build the matrices and plus-minus
for(t in 1:T) {

  ## design matrix for players        
  pht <- PH[t,!is.na(PH[t,])]
  pat <- PA[t,!is.na(PA[t,])]
  XP[t, pht] <- 1
  XP[t, pat] <- -1

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
  } else {
    Ppm[pht] <- Ppm[pht] - 1
    Ppm[pat] <- Ppm[pat] + 1
  }

  ## progress meter
  if(t %% 1000 == 0) 
        cat(t, " of ", T, " goals processed\n", sep="")
}

## back to matrix
XS <- as.matrix(XS)

## see which cols are all the same, and remove from df
same <- which(apply(XP, 2, var) == 0)
XP <- XP[,-same]
uN2 <- uniqueNames[-same]
pos2 <- positions[-same]
Ppm <- Ppm[-same]

## extra cleaning and output for gamlr
library(Matrix, lib=LIB)
onice <- Matrix(XP,sparse=TRUE)
colnames(onice) <- uN2
player <- data.frame(uN2, pos2, Ppm)
colnames(player) <- c("name", "position", "plus.minus")
config <- Matrix(XS,sparse=TRUE)
goal <- data.frame(whoscored=goals$g,
      season=goals$season, 
      team.away=goals$awayteam,
      team.home=goals$hometeam,
      period=goals$period,
      differential=goals$home.score-goals$away.score,
      session=goals$session,
      gamecode=goals$gcode)
rownames(config) <- rownames(goal) <- rownames(onice) <- 1:nrow(config)
save(goal,player,onice,config,compress="xz",file="results/hockey_goals.rda")