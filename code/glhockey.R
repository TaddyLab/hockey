#*******************************************************************************
#
# Chicago Hockey Analytics: Robert B, Gramacy and Matt Taddy
# Copyright (C) 2013, The University of Chicago
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
# Questions? Contact Robert B. Gramacy (rbgramacy@chicagobooth.edu), or
#                    Matt Taddy (taddy@chicagobooth.edu)
#
#*******************************************************************************


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










