# set the working directory
setwd("/Users/apple/Dropbox/Sen/hockey/shot\ and\ goal\ comparison") # user specific
source("args.R")

## load the roster object containing player/game info
load("data/roster.RData")

# function for collecting shots info
game.shots <- function(file, session)
{
  ## make sure the file exists
  if(!file.exists(file)) return(NULL)
  
  ## first read game file
  game <- read.csv(file, sep="|", comment="", quote="")
  
  ## get shots (including goals)
  shots <- game[which(game$etype == "GOAL" | game$etype == "SHOT"),]
  
  ## remove <= period 4 for regular season games
  if(session == "Regular")
    shots <- shots[shots$period <= 4,]
  
  ## check to make sure there were any shots in regulation time
  if(nrow(shots) == 0) return(NULL)
  
  ## instead check to make sure there are skaters on the ice
  ## (i.e., not shootout or penalty shot)
  shootout <- shots[,c(8:19,34:35)] != 1 
  w <- which(apply(shootout, 1, function(x) { sum(x) >= 8 }))
  eg <- shots[w,]
  
  ## remove un-needed columns (ref date, ev.players, ev.length)
  eg <- eg[,-c(3,21:23,33)]
  
  ## replace ev.team with home or away shot making team
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

## extract games, and prefix for location of files
G <- roster$games

## empty games data structure
games <- list()

## loop over games
for(i in 1:nrow(G)) {
  
  ## next if not valid or regular season
  if(! (G[i,]$valid)) { games[[i]] <- NULL; next }
  
  ## construct the file
  file <- sprintf("%s/%s-%s-gamerec.txt", EXT, G$season[i], G$gcode[i])
  
  ## get the game
  games[[i]] <- game.shots(file, G[i,]$session)
  
  ## progress meter
  if(i %% 100 == 0) 
    cat(i, " of ", nrow(G), " games processed\n", sep="")
}

## collapse list of data frames into one data frames
shots <- do.call("rbind", games)

## deal with types of columns
shots$g <- as.factor(shots$g)
shots$hometeam <- as.factor(shots$hometeam)
shots$awayteam <- as.factor(shots$awayteam)

## rename the g column
colnames(shots)[30] <- "ev.side"

## save to file
save(shots, file="results/nhlscrapr_shots.rda")

## now build design matrix and response -- 

## shot indicator(s)
Y <- shots$ev.side == "HOME"

## create unique names --
## this is overkill given the new roster structure
nplayers <- nrow(roster$roster.unique)
uID <- 1:nplayers
uN <- paste(roster$roster.unique$first, roster$roster.unique$last, sep="_")
positions <- uniqueNames <- rep(NA, length(unique(uID)))
uniqueNames[uID] <- uN # list of player names
positions[uID] <- roster$roster.unique$pos # list of player positions

## home and away player indicators
PA <- as.matrix(shots[,c(7:12,28)]) # away team (a1-a6 and away.G)
PA[PA == 1] <- NA # only have players that are on ice
PH <- as.matrix(shots[,c(13:18,29)]) # home team (h1-h6 and home.G)
PH[PH == 1] <- NA # only have players that are on ice

## build design matrix and make Plus-minus comparison
T <- nrow(PH)
# library for doing parallization
library(doMC,lib=LIB)
library(Matrix,lib=LIB)
registerDoMC(NC)

# list of files for output (number of files=number of cores)
flist_XP <- sprintf("XP_%s.mm", 1:NC)
flist_XS <- sprintf("XS_%s.mm", 1:NC)
# seperate the works to each core
chunk <- round(seq.int(0,T,length=NC+1))
# update of the design matrix for players and special teams
# the general idea is to update each row of design matrix according to shots info
# and output each row to the file and read back in as the design matrix
# we only output info about nonzero elements incluing the row, col and value
# in this way, we can read them back as sparse matrices to avoid memory issues
# my computer has 4 cores, it takes 1 hour to run the following code
ptm <- proc.time()

# prevent scintific notations or numbers like (1e+5)
options(scipen=10)
matrix_update <- foreach(i=1:NC) %dopar% {
  # number of rows
  num_row <- chunk[i+1]-chunk[i]
  # head of the Matrix Making file
  cat("%%MatrixMarket matrix coordinate integer general",file=flist_XP[i], sep="\n",append=TRUE)
  cat("%%MatrixMarket matrix coordinate integer general",file=flist_XS[i], sep="\n",append=TRUE)
  # output the dimensions of the matrices
  cat(c(num_row,nplayers,num_row*nplayers),file=flist_XP[i],sep=" ",append=TRUE)
  cat(c(num_row,7,num_row*7),file=flist_XS[i],sep=" ",append=TRUE)
  cat("\n",file=flist_XP[i],append=TRUE)
  cat("\n",file=flist_XS[i],append=TRUE)
  
  # separate the works to each core
  for(j in (chunk[i]+1):chunk[i+1]){
    # indicator of rows
    t = j - num_row*(i-1)
    # onice players for the home team and away team
    pht <- PH[j,!is.na(PH[j,])]
    pat <- PA[j,!is.na(PA[j,])]
    # data frame for output
    # first column represents the row, second column represents the column, third column represents the nonzero value
    out_XP <- data.frame(cbind(rep(t,length(pat)+length(pht)),c(pht,pat),c(rep(1,length(pht)),rep(-1,length(pat)))))
    # write to the file
    write.table(out_XP,file=flist_XP[i],sep=" ",eol = "\n",append=TRUE,row.names=FALSE,col.names=FALSE)
    
    # update the design matrix for special team
    # data frame for output
    out_XS <- data.frame(row = integer(), col = integer(), val = integer())
    # number of players on ice for home team
    spht <- sum(!is.na(PH[j,1:6])) 
    # number of players on ice for away team
    spat <- sum(!is.na(PA[j,1:6])) 
    # determine whether there is a special team case
    # here the columns (col=1...) correspond to: "S6v5", "S6v4", "S6v3", "S5v4", "S5v3", "S4v3", "SNG"
    if(spht == 6) {
      if(spat == 5) out_XS <- rbind(out_XS,data.frame(row=t,col=1,val=1))
      else if(spat == 4) out_XS <- rbind(out_XS,data.frame(row=t,col=2,val=1))
      else if(spat == 3) out_XS <- rbind(out_XS,data.frame(row=t,col=3,val=1))
    } else if(spht == 5) {
      if(spat == 6) out_XS <- rbind(out_XS,data.frame(row=t,col=1,val=-1))
      else if(spat == 4) out_XS <- rbind(out_XS,data.frame(row=t,col=4,val=1))
      else if(spat == 3) out_XS <- rbind(out_XS,data.frame(row=t,col=5,val=1))
    } else if(spht == 4) {
      if(spat == 3) out_XS <- rbind(out_XS,data.frame(row=t,col=6,val=1))
      else if(spat == 5) out_XS <- rbind(out_XS,data.frame(row=t,col=4,val=-1))
      else if(spat == 6) out_XS <- rbind(out_XS,data.frame(row=t,col=2,val=-1))
    } else if(spht == 3) {
      if(spat == 4) out_XS <- rbind(out_XS,data.frame(row=t,col=6,val=-1))
      else if(spat == 5) out_XS <- rbind(out_XS,data.frame(row=t,col=5,val=-1))
      else if(spat == 6) out_XS <- rbind(out_XS,data.frame(row=t,col=3,val=-1))
    }
    ## checking for pulled-goalie situation at the end of the game
    if(shots[j,]$session == "Regular" 
       && shots[j,]$period == 3 && shots[j,]$seconds > 3480) {
      if(is.na(PH[j,7]) && (shots[j,]$home.score < shots[j,]$away.score)) {
        out_XS <- rbind(out_XS,data.frame(row=t,col=7,val=1))}
      if(is.na(PA[j,7]) && (shots[j,]$home.score > shots[j,]$away.score)) {
        out_XS <- rbind(out_XS,data.frame(row=t,col=7,val=-1))}
    }
    # write to the file
    write.table(out_XS,file=flist_XS[i],sep=" ",eol = "\n",append=TRUE,row.names=FALSE,col.names=FALSE,quote=FALSE)
  }
}
# set back to default
options(scipen=0)
# read the files as lists
proc.time() - ptm
named.list_XP <- lapply(flist_XP, readMM)
named.list_XS <- lapply(flist_XS, readMM)
# combine to get a single matrix
XP <- do.call(rBind,named.list_XP)
XS <- do.call(rBind,named.list_XS)


## plus-minus calculation
# this takes 12 seconds
Ppm <- rep(0, nplayers) ## players

ptm <- proc.time()
for(t in 1:T){
  pht <- PH[t,!is.na(PH[t,])]
  pat <- PA[t,!is.na(PA[t,])]
  ## plus-minus
  if(Y[t]) { ## home goal
    Ppm[pht] <- Ppm[pht] + 1
    Ppm[pat] <- Ppm[pat] - 1
  } else {
    Ppm[pht] <- Ppm[pht] - 1
    Ppm[pat] <- Ppm[pat] + 1
  }
}
proc.time() - ptm

# function for calculating the variance of columns in a matrix
colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
    sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}

## see which cols are all the same, and remove from df
ptm <- proc.time()
same <- which(colVars == 0)
XP <- XP[,-same]
uN2 <- uniqueNames[-same]
pos2 <- positions[-same]
Ppm <- Ppm[-same]
proc.time() - ptm

## extra cleaning and output for gamlr
library(Matrix, lib=LIB)
onice_shots <- XP
colnames(onice_shots) <- uN2
player_shots <- data.frame(uN2, pos2, Ppm)
colnames(player_shots) <- c("name", "position", "plus.minus")
config_shots <- XS
colnames(config_shots) <- c("S6v5", "S6v4", "S6v3", "S5v4", "S5v3", "S4v3", "SNG")
shot <- data.frame(whoshoted=shots$ev.side,
                   season=shots$season, 
                   team.away=shots$awayteam,
                   team.home=shots$hometeam,
                   period=shots$period,
                   differential=shots$home.score-shots$away.score,
                   session=shots$session,
                   gamecode=shots$gcode)
rownames(config_shots) <- rownames(shot) <- rownames(onice_shots) <- 1:nrow(config_shots)
save(shot,player_shots,onice_shots,config_shots,compress="xz",file="results/hockey_shots_finalversion.rda")