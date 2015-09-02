## This code is used to calculate the team involvement rate (seasonal and career) for each player in the nhl league
## We have 2412 players and 31 nhl teams (note: ATL was the former of WPG, we treat them separately)
## The output is eleven 2412*31 matrices (10 seasons and 1 career)
# e.g. for player AARON_GAGNON, the career team involvement matrix shows that he spent more than half of his career
# in DAL (weight=0.5526316) and he spent the rest his career in WPG (weight=0.4473684)

# set the working directory (user specific)
setwd("/Volumes/HDD/Dropbox/Sen/hockey/salary")
library(Matrix)
library(MatrixModels)
# load the shots data set
load(file="data/previous/hockey_shots.rda")
# all teams 
nhlteams <- levels(factor(shot$team.away))
# number of teams
nteams <- length(nhlteams)
# name of the players
nhlplayers <- colnames(onice_shots)
# number of players
nplayers <- length(nhlplayers)
# seasons
season <- levels(as.factor(shot$season))
# the team involving rates for players in each season are stored in a list
# 'tir' stands for team involving rate
tir_season <- list()
## team involment counts for each season
for(i in 1:length(season)){
  # shots info for the specific season
  onice_season_i <- onice_shots[which(shot$season == season[i]),]
  shot_season_i <- shot[which(shot$season == season[i]),]
  # initialize a matrix to store total involvement counts in this season
  totinv <- matrix(0,nrow=nplayers,ncol=nteams)
  rownames(totinv) <- nhlplayers
  colnames(totinv) <- nhlteams
  # summary of a sparse matrix
  # entries are 'i'-which obs, 'j'-which player, 'x'-plus or minus 1 
  smy <- summary(onice_season_i)
  # for each shot record, figure out the team that the player was in
  # remember x=1 represents home shot and x=-1 represents away shot
  smy <- cbind(smy,t=ifelse(smy$x<0,shot_season_i[smy$i,]$team.away,shot_season_i[smy$i,]$team.home))
  # iterate over all the teams
  for(t in 1:nteams){
    # shot records correspond to team t
    smy_team_i <- smy[which(smy$t==t),]  
    # WPG was not in the league in early seasons, check smy_team_i has record
    if(nrow(smy_team_i)>0){
      # for each player, figure out the games he has played in terms of game codes
      ugcode_player <- tapply(shot_season_i$gamecode[smy_team_i$i],smy_team_i$j,function(g) unique(g)) 
      # update the involvement matrix
      totinv[as.numeric(names(ugcode_player)),t] <- as.numeric(summary(ugcode_player)[,1])
    }
  }
  # convert into a sparse matrix
  tir_season[[i]] <- Matrix(totinv,sparse=TRUE)
}
# check the maximum number of games that a player has played in a season
for(i in 1:length(tir_season)){
  print(max(rowSums(tir_season[[i]])))
}

# sum the seasonal team involvement counts to get the career counts
tir_career <- Reduce('+', tir_season)
# add tir_career to the list of tir_season
tir <- c(tir_season,tir_career)
# name the elements in the list
names(tir) <- c(season,'career')
# weight the team involvement counts
for(i in 1:length(tir)){
  tir[[i]] <- sweep(tir[[i]],1,rowSums(tir[[i]]),'/')
  tir[[i]][is.na(tir[[i]])] <- 0.0
  tir[[i]] <- Matrix(tir[[i]],sparse=TRUE)
}
# output the team involving rates
tir_summary_season <- lapply(tir,summary)
tir_summary_all <- do.call(rbind,tir_summary_season)
tir_summary_all <- cbind(tir_summary_all, season=rep(names(tir),sapply(tir_summary_season, nrow)))
tir_df <- data.frame(player=nhlplayers[tir_summary_all$i], team=nhlteams[tir_summary_all$j],
                     weight=tir_summary_all$x, season=tir_summary_all$season)
write.table(tir_df,row.names=FALSE,sep="|",file="results/team_inv_rate.txt")

##############################################################################################################################
## the following code shows how to read the matrices back from the file
## and compare with the original ones
tir_df_check <- read.table("results/team_inv_rate.txt",colClasses=c("factor","factor","numeric","factor"),header=TRUE,sep="|")
tir_summary_check <- split(tir_df_check,tir_df_check$season)
players_check <- levels(tir_df_check$player)
teams_check <- levels(tir_df_check$team)
tir_check <- list()
for(i in 1:length(tir_summary_check)){
  i_1 = as.numeric(tir_summary_check[[i]]$player)
  j_1 = as.numeric(tir_summary_check[[i]]$team)
  x_1 = tir_summary_check[[i]]$weight
  tir_check[[i]] <- sparseMatrix(i=i_1,j=j_1,x=x_1,dims=c(length(players_check),length(teams_check)), 
                                 dimnames=list(players_check,teams_check))
}
# check whether the matrices we read from files correspond to the ones we had
for(i in 1:length(tir)){
  # sort the data frame with alphabetical order of player names
  tir[[i]] <- tir[[i]][order(rownames(tir[[i]])),]
  if(all.equal(tir[[i]],tir_check[[i]])){print('success')}
}