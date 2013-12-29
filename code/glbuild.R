## building goal data
## you need to create a sim link in the main directory to 
## wherever you store the nhlgames hockey scrape
## e.g., `ln -s /project/rbgramacy/hockey external'

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
            file=sprintf("external/nhlgames/%s-gamerec.txt",i), 
            session=gmat[i,]$session)
  print(i)
  }
  do.call(rbind,glist)
}
  
## grab games, break into list, and par-loop
load("data/roster.RData")
G <- roster$games
rownames(G) <- sprintf("%s-%s", G$season, G$gcode)
G <- lapply(unique(G$season), function(s) G[G$season==s,])
clusterExport(cl,"game.goals")
goals <- do.call(rbind,parLapply(cl,G,season.goals))

## deal with types of columns 
goals$g <- as.factor(goals$g)
## first fix team names
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
names(Y) <- rownames(goals)
ngoals <- nrow(goals)

## unique player info
nplayers <- nrow(roster$roster.unique)
upnames <- paste(roster$roster.unique$first, 
            roster$roster.unique$last, sep="_")
positions <- roster$roster.unique$pos

## function to build designsÏ
design.goals <- function(gmat){
  require(Matrix)

  ## Design matrices
  T <- nrow(gmat)
  XP <- Matrix(0, nrow=T, ncol=length(upnames),
        dimnames=list(rownames(gmat),upnames))
  XS <- Matrix(0, ncol=7, nrow=T,
    dimnames=list(rownames(gmat),
      c("S6v5", "S6v4", "S6v3", "S5v4", "S5v3", "S4v3", "SNG")))

  ## skater and goalie columns
  hp <- c(paste("h",1:6,sep=""),"home.G")
  ap <- c(paste("a",1:6,sep=""),"away.G")

  ## build the matrices and plus-minus
  for(t in 1:T) {

    ## players on ice    
    pht <- gmat[t,hp]
    pat <- gmat[t,ap]

    ## player design rows
    XP[t, pht[pht!=1]] <- 1
    XP[t, pat[pat!=1]] <- -1

    ## number of actual skaters
    sh <- sum(pht[1:6]!=1)
    sa <- sum(pat[1:6]!=1)

    ## design matrix for special teams
    HvA <- sprintf("S%dv%d", sh, sa)
    if(HvA %in% colnames(XS)) XS[t,HvA] <- 1
    AvH <- sprintf("S%dv%d", sa, sh)
    if(AvH %in% colnames(XS)) XS[t,AvH] <- -1
  
    ## checking for pulled-goalie situation at the end of the game
    if(gmat[t,]$session == "Regular" 
        && gmat[t,]$period == 3 && gmat[t,]$seconds > 3480){
          if( (pht[7]==1) && (gmat[t,]$home.score < gmat[t,]$away.score)) 
            XS[t,"SNG"] <- 1
          if( (pat[7]==1) && (gmat[t,]$home.score > gmat[t,]$away.score)) 
            XS[t,"SNG"] <- -1 
        }

    ## progress meter
    if(t %% 1000 == 0) 
        cat(t, " of ", T, " goals processed\n", sep="")
  }

  cBind(XS,XP)
}


## split into chunks and par-loop
clusterExport(cl, "upnames")
chunk <- round(seq.int(0,ngoals,length=NC+1))
G <- lapply(1:NC, function(i) goals[(chunk[i]+1):chunk[i+1],])
X <- do.call(rBind, parLapply(cl,G,design.goals))
XS <- X[,1:7]
XP <- X[,-(1:7)]

## stop
stopCluster(cl)

## remove ne'r occurs
allzero <- which(colSums(XP!=0)==0)
XP <- XP[,-allzero]
positions <- positions[-allzero]

## goals
goal <- data.frame(
    whoscored=goals$g,
    season=as.character(goals$season), 
    awayteam=as.character(goals$awayteam),
    hometeam=as.character(goals$hometeam),
    period=goals$period,
    differential=goals$home.score-goals$away.score,
    session=as.character(goals$session),
    seconds=goals$seconds,
    gcode=as.character(goals$gcode),
    stringsAsFactors=FALSE)

## players
active <- goal$season[XP@i[tail(XP@p,-1)]+1]
lastyear <- as.numeric(substr(active,5,8))
entry <- goal$season[XP@i[tail(XP@p,-1)+1]+1]
firstyear <- as.numeric(substr(entry,1,4))

player <- data.frame(position=positions,
            plus.minus=colSums(XP),
            entry=entry,active=active,
            firstyear=firstyear,lastyear=lastyear,
            stringsAsFactors=FALSE)
rownames(player) <- colnames(XP)

## add coach info
source("code/coaches.R")
hc <- coach[cbind(goal$hometeam,goal$season)]
ac <- coach[cbind(goal$awayteam,goal$season)]

## who is the missing washington coach?
cf <- relevel(factor(c(hc,ac),levels=unique(c(hc,ac))),".")
cw <- matrix(as.numeric(cf)-1,ncol=2)
## turn binary
XC <- Matrix(0,nrow=ngoals,ncol=nlevels(cf)-1,
  dimnames=list(goal$gcode,levels(cf)[-1]))
XC[cbind(1:ngoals,cw[,1])] <- 1
XC[cbind(1:ngoals,cw[,2])] <- -1
colnames(XC) <- paste("COACH",colnames(XC),sep="_")

## add team info
teams <- sort(unique(c(goal$hometeam,goal$awayteam)))
seasons <- sort(unique(goal$season))
team.season <- paste(rep(teams, length(seasons)), 
  rep(seasons, each=length(teams)), sep=".")
XT <- Matrix(0, nrow=ngoals, ncol=length(team.season),
    dimnames=list(goal$gcode,team.season))
tc <- paste(c(goal$hometeam,goal$awayteam),rep(goal$season,2), sep=".")
tf <- factor(tc,levels=team.season)
tw <- matrix(as.numeric(tf),ncol=2)
XT[cbind(1:ngoals,tw[,1])] <- 1
XT[cbind(1:ngoals,tw[,2])] <- -1

## save
save(XS,XT,XC,XP,Y,player,goal,teams,seasons,file="data/nhldesign.rda", compress=FALSE)



