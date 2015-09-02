## This code is used to investigate the influence of player performance and team effects on player salaries

# set the working directory (user specific)
setwd("/Users/apple/Dropbox/Sen/hockey/salary")
library(Matrix)
library(gamlr)
# read in the matrices of team involvement rates
tir_df <- read.table("results/team_inv_rate.txt",colClasses=c("factor","factor","numeric","factor"),header=TRUE,sep="|")
tir_summary <- split(tir_df,tir_df$season)
players <- levels(tir_df$player)
teams <- levels(tir_df$team)
tir <- list()
for(i in 1:length(tir_summary)){
  i_1 = as.numeric(tir_summary[[i]]$player)
  j_1 = as.numeric(tir_summary[[i]]$team)
  x_1 = tir_summary[[i]]$weight
  tir[[i]] <- sparseMatrix(i=i_1,j=j_1,x=x_1,dims=c(length(players),length(teams)),dimnames=list(players,teams))
}

# salary model regression using data from season 2002-2003 to season 2012-2013
# read in the salary data
salary <- read.table("data/nhlsalaries_bhz_nc.txt", header=TRUE, sep='|', quote="")
rownames(salary) <- salary$Name
# read in the player beta (goals model)
player_beta_df <- read.table("/Users/Sen/Documents/Hockey\ Salary\ Analysis/data/previous/playereffect_shot.txt",
                             colClasses=c("factor","factor","numeric"),header=TRUE,sep="|")
player_beta <- sparseMatrix(i=as.numeric(player_beta_df$player),j=as.numeric(player_beta_df$season),
                            x=player_beta_df$effect,dimnames=list(levels(player_beta_df$player),levels(player_beta_df$season)))
# merge the info we have and build up a data frame for regression
# we have 1668 common players
common_players <- intersect(rownames(player_beta),salary$Name)
# salary info from season 2002 to season 2012
salary <- salary[salary$Name %in% common_players,2:11]
# salaries per year that the player played
salary <- cbind(salary,rowSums(salary)/rowSums(salary!=0))
colnames(salary)[ncol(salary)] <- 'avg.sal'
player_beta <- player_beta[rownames(player_beta) %in% common_players,]
for(i in 1:length(tir)){
  tir[[i]] <- tir[[i]][rownames(tir[[i]]) %in% common_players,]
  # sort the data frame with alphabetical order of player names
  tir[[i]] <- tir[[i]][order(rownames(tir[[i]])),]
}
# sanity check whether the rownames of the data frames (players) match 
if(identical(rownames(player_beta),rownames(salary)) & identical(rownames(salary),rownames(tir[[1]]))){
  print('rownames of data frames match')
}

# check the number of players of non-zero salaries in each season
for(i in 1:ncol(salary)){
  cat(sprintf("For season %s, there are %d players with non-zero salaries, among which %d players ever played in a team.\n", 
              colnames(player_beta)[i],sum(salary[,i]!=0),length(intersect(which(salary[,i]!=0),which(rowSums(tir[[i]])!=0)))))
}



########### simple model 1: log(salary) = team + player_beta(shots)
########### simple model 2: log(salary) = team + player_beta + team*player_beta
########### term 'team*player_beta' is penalized while other 2 are free
reg_lm <- list()
reg_lm_winter <- list()
reg_rg <- list()
coef_pe <- c()
r2_pe <- c()
for(i in 1:length(tir)){
  # players with non-zero salaries (played for that season)
  p_nz <- intersect(which(salary[,i]!=0),which(rowSums(tir[[i]])!=0))
  # linear regression: log(salary)~team_involvement_weights * player_beta
  y <- log(salary[p_nz,i])
  t <- as.matrix(tir[[i]][p_nz,])
  b <- player_beta[p_nz,i]
  # model 1
  reg_lm[[i]] <- lm(y~b+t)
  coef_pe <- c(coef_pe,coef(reg_lm[[i]])[1])
  r2_pe <- c(r2_pe,summary(reg_lm[[i]])$r.squared)
  # model 2
  x <- model.matrix( ~ b + t + t:b  - 1)
  reg_lm_winter[[i]] <- lm(y~b*t)
  reg_rg[[i]] <- cv.gamlr(x=x, y=y, family="gaussian", standardize=FALSE, gamma=0, 
                          verb=FALSE, free=1:(ncol(t)+1),nfold=10)
  # coeffiencts for the interaction terms
  coef_inter <- coef(reg_rg[[i]])[34:64]
  if(sum(coef_inter!=0)==0){
    print('Zero interaction')
  }
  #print(anova(reg_lm[[i]],reg_lm_winter[[i]])$Pr[2])
}
s <- seq(1,10)
old.par <- par(mfrow=c(1, 2))
plot(s,coef_pe[-11],"l",xlab="season",ylab="coef of player effect")
abline(a=coef_pe[11],b=0,col=2,lty=2,lwd=2)
plot(s,r2_pe[-11],"l",xlab="season",ylab="R square")
abline(a=r2_pe[11],b=0,col=2,lty=2,lwd=2)
par(old.par)





g <- influence(reg_lm[[11]])
pnames <- pnames_lm[[11]]
halfnorm(g$hat,ylab="Leverages",nlab=4,labs=pnames)


