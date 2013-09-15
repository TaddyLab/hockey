### Comparison of the results by using goals data and shots data

# Set the working directory
setwd("/Users/apple/Dropbox/Sen/hockey/shots") # user specific
# Package 'gamlr' version 1.11
library(gamlr)

## Read in the data sets
# Goals data
data(hockey)
# Rename the variables
player_goals <- player
config_goals <- config
onice_goals <- onice
# Remove unnecessary variables
rm(player, config, onice)
# Shots data
load("results/hockey_shots.rda") # can be found in Github

## Build up the design matrices and response vectors
x_goals <- cBind(config_goals,onice_goals)
# y=1 for Home goals and y=0 for Away goals
y_goals <- as.numeric(goal$who=="HOME") 
x_shots <- cBind(config_shots,onice_shots)
# y=1 for Home shots and y=0 for Away shots
y_shots <- as.numeric(shot$who=="HOME") 

## Find the optimal values of gamma and lambda
val_AIC <- val_BIC <- matrix(0, ncol=100, nrow=12)
i <- 1
# take 12 diff values of gamma and 100 lambda
for (gamma in c(0,10^(-5:5))){
  fit_g_goals <- gamlr(x=x_goals, y=y_goals, family="binomial", lambda.start=exp(-6),
                       standardize=FALSE, gamma=gamma, verb=FALSE, free=1:ncol(config_goals))
  val_AIC[i,] <- rev(log(AIC(fit_g_goals))) # reverse the sequence for output
  val_BIC[i,] <- rev(log(BIC(fit_g_goals)))
  i <- i+1
}
# name the rows and columns
colnames(val_AIC) <- colnames(val_BIC) <- rev(log(fit_g_goals$lambda))
rownames(val_AIC) <- rownames(val_BIC) <- c(0,10^(-5:5))
# find the gamma and lambda giving the minimum AIC or BIC
which(val_AIC == min(val_AIC), arr.ind = TRUE) # gamma=10^(-5), log(lambda)=-9.53528, AIC=11.21851
which(val_BIC == min(val_BIC), arr.ind = TRUE) # gamma=10, log(lambda)=-6.51168, BIC=11.23818

# write the results in a file for plotting 2-D map in gnuplot
file_AIC <- "plots/AIC_goals.txt"
file_BIC <- "plots/BIC_goals.txt"
for (i in 1:100){
  for (j in 1:12){
    output_AIC <- c(i,j,val_AIC[j,i])
    output_BIC <- c(i,j,val_BIC[j,i])
    cat(output_AIC,file=file_AIC,"\n",append=TRUE)
    cat(output_BIC,file=file_BIC,"\n",append=TRUE)
  }
  cat(NULL,file=file_AIC,"\n",append=TRUE)
  cat(NULL,file=file_BIC,"\n",append=TRUE)
}

## Do the above analysis for the shots data
val_AIC <- val_BIC <- matrix(0, ncol=100, nrow=12)
i <- 1
# take 12 diff values of gamma and 100 lambda
for (gamma in c(0,10^(-5:5))){
  fit_g_shots <- gamlr(x=x_shots, y=y_shots, family="binomial", lambda.start=exp(-6),
                       standardize=FALSE, gamma=gamma, verb=FALSE, free=1:ncol(config_shots))
  val_AIC[i,] <- rev(log(AIC(fit_g_shots)))
  val_BIC[i,] <- rev(log(BIC(fit_g_shots)))
  i <- i+1
}
# name the rows and columns
colnames(val_AIC) <- colnames(val_BIC) <- rev(log(fit_g_shots$lambda))
rownames(val_AIC) <- rownames(val_BIC) <- c(0,10^(-5:5))
# find the gamma and lambda giving the minimum AIC or BIC
which(val_AIC == min(val_AIC), arr.ind = TRUE) # gamma=10, log(lambda)=-10.60517, AIC=13.73154
which(val_BIC == min(val_BIC), arr.ind = TRUE) # gamma=10, log(lambda)=-8.83753, BIC=13.73253

# write the results in a file for plotting 2-D map in gnuplot
file_AIC <- "plots/AIC_shots.txt"
file_BIC <- "plots/BIC_shots.txt"
for (i in 1:100){
  for (j in 1:12){
    output_AIC <- c(i,j,val_AIC[j,i])
    output_BIC <- c(i,j,val_BIC[j,i])
    cat(output_AIC,file=file_AIC,"\n",append=TRUE)
    cat(output_BIC,file=file_BIC,"\n",append=TRUE)
  }
  cat(NULL,file=file_AIC,"\n",append=TRUE)
  cat(NULL,file=file_BIC,"\n",append=TRUE)
}

## Fit the models by using the optimal parameters chosen above
fit_goals <- gamlr(x=x_goals, y=y_goals, family="binomial",
                   standardize=FALSE, gamma=10, verb=TRUE, free=1:ncol(config_goals))
fit_shots <- gamlr(x=x_shots, y=y_shots, family="binomial",
                   standardize=FALSE, gamma=10, verb=TRUE, free=1:ncol(config_shots))

## BIC selected coefficients
# coefficients for players
B_goals <- as.matrix(coef(fit_goals)[-c(1:8),]) 
B_shots <- as.matrix(coef(fit_shots)[-c(1:8),])
# How many players get nonzero coefficients?
num_nonzero_goals <- sum( B_goals !=0); num_nonzero_goals # gives 7 players
num_nonzero_shots <- sum( B_shots !=0); num_nonzero_shots # gives 34 players 
# Non-zero player effects in the minimum BIC model
# order the non-zero player effects
B_goals_order <- B_goals[order(-abs(B_goals)),]
# create a data frame for the non-zero player effects 
# including the name, position, plus.minus value and the beta coefficient
nonzero_goals <- cbind(player_goals[match(names(B_goals_order)[1:num_nonzero_goals], player_goals[,1]),],
                       B_goals_order[1:num_nonzero_goals])
colnames(nonzero_goals)[4] <- "coefficent.beta"
nonzero_goals

# do the same thing for shots data
B_shots_order <- B_shots[order(-abs(B_shots)),]
nonzero_shots <- cbind(player_shots[match(names(B_shots_order)[1:num_nonzero_shots], player_shots[,1]),],
                       B_shots_order[1:num_nonzero_shots])
colnames(nonzero_shots)[4] <- "coefficent.beta"
# players with positive beta appear first
rbind(nonzero_shots[which(nonzero_shots$coefficent.beta >0),],
      nonzero_shots[which(nonzero_shots$coefficent.beta <0),])


## Plot of the regularization paths
# Color players by positions
# Green-C, Blue-D, Gray-G, Red-L, Red-R
color_goals <- c("Green","Blue","Gray","Red","Red")[player_goals$position]
plot(fit_goals, xlim=c(-9.5,-6), ylim=c(-2,1.5),
     col=c(0,
           rep(0,ncol(config_goals)),
           color_goals))
legend(-7.5,-1, c("Center","Defenseman","Goaltender","Winger"), 
       col=c("Green","Blue","Gray","Red"), lty=1, lwd=2, cex=0.8)

color_shots <- c("Green","Blue","Gray","Red","Red")[player_shots$position]
plot(fit_shots, ylim=c(-1.2,0.5),
     col=c(0,
           rep(0,ncol(config_shots)),
           color_shots))
legend(-10.5,-0.7, c("Center","Defenseman","Goaltender","Winger"), 
       col=c("Green","Blue","Gray","Red"), lty=1, lwd=2, cex=0.8)

## Value of log(lambda) selected by AIC or BIC
log(fit_goals$lambda)[which.min(BIC(fit_goals))] # BIC
log(fit_goals$lambda)[which.min(AIC(fit_goals))] # AIC

log(fit_shots$lambda)[which.min(BIC(fit_shots))] # BIC
log(fit_shots$lambda)[which.min(AIC(fit_shots))] # AIC

## Predict the last goal of the 2013 playoffs (silly)
goal[fit_goals$n,]
predict(fit_goals,x_goals[fit_goals$n,,drop=FALSE],type="response")

shot[fit_shots$n,]
predict(fit_shots,x_shots[fit_shots$n,,drop=FALSE],type="response")