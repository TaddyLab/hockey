## Shot quality prediction
# Set the working directory
setwd("/Users/apple/Dropbox/Sen/hockey/shot\ quality\ prediction") # user specific
# Shots data
load("../shot\ and\ goal\ comparison/results/nhlscrapr_shots.rda") # user specific
# librarys
library(Matrix)
library(glmnet)

# pull out the info including indicator of goals, distance, shot type(factor), session(factor)
# (x and y coordinates are not available)
shotsinfo <- data.frame(as.numeric(shots$etype == "GOAL"),
                        shots$distance, shots$type, factor(shots$session))
shotsinfo <- na.omit(shotsinfo)
# set the names of columns
colnames(shotsinfo) <- c("indicator.goal", "distance", "type", "session")
# Correct two shot types
shotsinfo$type[shotsinfo$type == 'Tip-In'] <- 'Tip-in'
shotsinfo$type[shotsinfo$type == 'rist'] <- 'Wrist'
# delete observations with a misunderstanding shots type
shotsinfo <- shotsinfo[-which(shotsinfo$type == 'Unsuccessful Penalty Shot Wrist'),]
shotsinfo$type <- factor(shotsinfo$type)
# Summary of the data
str(shotsinfo)

# Model Matrix
x <- sparse.model.matrix(~ session + poly(distance,5)*type, data=shotsinfo)
# cross validation
y <- shotsinfo$indicator.goal
shotslogit.cv <- cv.glmnet(x, y, family="binomial",standardize=FALSE, nfolds=10,
                           lambda=exp(seq(-8,-30,length.out=100)))
# AIC and BIC
source("../data\ mining\ class/functions/IC.R")
aic <- glmnetIC(shotslogit.cv$glmnet)
bic <- glmnetIC(shotslogit.cv$glmnet, k=log(nrow(x)))
## plot the AIC and BIC
plot(log(shotslogit.cv$lambda), aic, pch=21, bg="orange", xlab="log(Lambda)", main="AIC")
abline(v=log(shotslogit.cv$lambda[which.min(aic)]), col="navy", lty=2)
plot(log(shotslogit.cv$lambda), bic, pch=21, bg="orange", xlab="log(Lambda)", main="BIC")
abline(v=log(shotslogit.cv$lambda[which.min(bic)]), col="navy", lty=2)

# plot the binomial deviance
plot(shotslogit.cv)
abline(v=log(shotslogit.cv$lambda[which.min(aic)]), col="navy", lty=2)
abline(v=log(shotslogit.cv$lambda[which.min(bic)]), col="navy", lty=2)
# plot the regularization path
plot(shotslogit.cv$glm, xv="lambda") 
abline(v=log(shotslogit.cv$lambda[which.min(aic)]), col="navy", lty=2)
abline(v=log(shotslogit.cv$lambda[which.min(bic)]), col="navy", lty=2)

# Non-zero coefficients 
beta <- as.matrix(coef(shotslogit.cv))
beta[which(beta!=0),]
