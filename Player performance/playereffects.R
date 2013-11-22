## set the working directory (user specific)
setwd("/Users/apple/Dropbox/Sen/hockey/player\ effect")
# load the 'gamlr' package
library(gamlr)
library(Matrix)

# Read in the data set
load("../shot\ and\ goal\ comparison/results/hockey_goals.rda")

## Build up the design matrices and response vectors
x <- cBind(config,onice)
# y=1 for Home goals and y=0 for Away goals
y <- as.numeric(goal$who == "HOME") 

# declare lists to store the by-year design matrices and response vectors
x_byyear <- list()
y_byyear <- list()

j = 1
# parse the big design matrix in terms of season
for (i in levels(factor(goal$season))){
  ind <- which(goal$season == i)
  x_byyear[[j]] <- x[ind,]
  y_byyear[[j]] <- y[ind]
  j <- j+1
}

# gamlr (lasso) regression on the entire career
fit_career <- gamlr(x=x, y=y, family="binomial", standardize=FALSE, gamma=0, 
                    verb=FALSE, free=1:ncol(config))
# coefficients which give the minimum AIC
# pull out the coefficients for players
beta_career <- coef(fit_career, which.min(AIC(fit_career)))[-c(1:8),]

# gamlr (lasso) regression on each season
# gives an error of infinite likelihood for i=10
beta_season <- list()
for (i in 1:length(x_byyear)){
  fit_season <- gamlr(x=x_byyear[[i]], y=y_byyear[[i]], family="binomial", standardize=FALSE, gamma=0, 
               verb=FALSE, free=1:ncol(config))
  beta_season[[i]] <- coef(fit_season, which.min(AIC(fit_season)))[-c(1:8),]
}

# beta matrix with dim=nplayers*(1+nseasons), 1 here represents career
beta <- Matrix(cbind(beta_career,do.call("cbind", beta_season)), sparse=TRUE)
colnames(beta) <- c("career",levels(factor(goal$season)))

# output the beta matrix to a file
# rows with all zero entries are deleted
beta_summary <- summary(beta) # including the row, col, val of nonzero elements
beta_df <- data.frame(player=rownames(beta)[beta_summary$i], season=colnames(beta)[beta_summary$j], 
                      effect=beta_summary$x)
write.table(beta_df,row.names=FALSE,sep="|",file="playereffect.txt")

# read it back to check
beta_df_check <- read.table("playereffect.txt",colClasses=c("factor","factor","numeric"),header=TRUE,sep="|")
beta_check <- sparseMatrix(i=as.numeric(beta_df_check$player),j=as.numeric(beta_df_check$season),
              x=beta_df_check$effect,dimnames=list(levels(beta_df_check$player),levels(beta_df_check$season)))