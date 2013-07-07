
#################################################################
## looking at various MAP estimators.  Uses pre-CRAN gamlr_1.x ##
#################################################################

### Data extraction and output for gamlr
## follows the naming steps in bobby's code

library(Matrix)
load("../data/nhlscrapr_logit_data.RData")

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

### analysis and model comparison
### clear everything and load clean

rm(list=ls())
load("../data/hockey.rda")

## run the two at lasso; 
## BIC should give exact same result
## CV will differ due to MC variance
library(glmnet)
library(gamlr)

x <- cBind(onice,player)
y <- as.numeric(goal$who=="HOME")

gltime <- system.time(fit <- cv.gamlr(x=x, y=y, 
						family="binomial", 
						standardize=FALSE,
        				varpen=0, 
        				pen.min.ratio=0.0001,
        				verb=TRUE) )	
coef(fit, s="min")
print(which.min(BIC(fit$gamlr)))

nettime <- system.time(fitnet <- cv.glmnet(x=x, y=y, 
						family="binomial", 
						standardize=FALSE))	
coef(fitnet, s="lambda.min")
devnet <- (1-fitnet$glmnet$dev.ratio)*fitnet$glmnet$nulldev
print(which.min( devnet + log(fitnet$glmnet$nobs)*(fitnet$nzero+1) ))

par(mfrow=c(1,2))
plot(fit)
plot(fitnet)

print(gltime-nettime)










