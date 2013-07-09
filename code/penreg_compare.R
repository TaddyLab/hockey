
#################################################################
## looking at various MAP estimators.  Uses pre-CRAN gamlr_1.x ##
#################################################################


### analysis and model comparison

load("../data/hockey.rda")

## run the two at lasso; 
## BIC should give exact same result
## CV will differ due to MC variance
library(glmnet, lib=LIB)
library(gamlr, lib=LIB)

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










