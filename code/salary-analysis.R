# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# nonparametric Bayesian regression (Figure 4 in CRC paper)
library(gamlr)
library(tgp)

nhldf <- readRDS("results/perfsaldf.rds")
nhldf$season <- factor(nhldf$season)
plot(sal ~ season, data=nhldf)

s <- 1:nrow(nhldf) #which( as.numeric(nhldf$season) <=3 )
plays <- factor(nhldf$player[s])
y <- log(tapply(nhldf$sal[s], plays, mean))

Xpm  <- as.matrix(tapply(nhldf$pm[s], plays, mean))
Xppm <- as.matrix(tapply(nhldf$ppm[s], plays, mean))
Xfp  <- as.matrix(tapply(nhldf$fp[s], plays, mean))
Xprob<- as.matrix(tapply(nhldf$prob[s], plays, mean))

pmgrid <- seq(-30,30,length=100)
fpgrid <- seq(0,1,length=100)

R <- 100

pmfit <- bcart(X=Xpm, Z=y, XX=pmgrid,R=R)
ppmfit <- bcart(X=Xppm, Z=y, XX=pmgrid,R=R)

fpfit <- bcart(X=Xfp, Z=y, XX=fpgrid,R=R)
probfit <- bcart(X=Xprob, Z=y, XX=fpgrid,R=R)

pdf(file="salreg-goals.pdf",width=8,height=3)
par(mfrow=c(1,2),mai=c(.9,.9,.1,.1))
plot(pmgrid[-100], pmfit$ZZ.mean[-100],type="l",lwd=2, bty="n", 
     xlab="plus minus", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(pmgrid, ppmfit$ZZ.mean,col="darkorange",lwd=2)
legend("topleft", bty="n", legend=c("partial","sample"),lwd=4, 
       col=c("darkorange","grey25"))
plot(fpgrid, fpfit$ZZ.mean,type="l",lwd=2, bty="n",
     xlab="for-probability", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(fpgrid, probfit$ZZ.mean,col="darkorange",lwd=2)
dev.off()


### same thing, but corsi
Xpm  <- as.matrix(tapply(nhldf$pm.corsi[s], plays, mean))
Xppm <- as.matrix(tapply(nhldf$ppm.corsi[s], plays, mean))
Xfp  <- as.matrix(tapply(nhldf$fp.corsi[s], plays, mean))
Xprob<- as.matrix(tapply(nhldf$prob.corsi[s], plays, mean))

pmgrid <- seq(-400,400,length=100)
fpgrid <- seq(0,1,length=100)

R <- 10

pmfit <- bcart(X=Xpm, Z=y, XX=pmgrid,R=R)
ppmfit <- bcart(X=Xppm, Z=y, XX=pmgrid,R=R)

fpfit <- bcart(X=Xfp, Z=y, XX=fpgrid,R=R)
probfit <- bcart(X=Xprob, Z=y, XX=fpgrid,R=R)

pdf(file="salreg-corsi.pdf",width=8,height=3)
par(mfrow=c(1,2),mai=c(.9,.9,.1,.1))
plot(pmgrid, pmfit$ZZ.mean,type="l",lwd=2, bty="n", 
     xlab="plus minus", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(pmgrid, ppmfit$ZZ.mean,col="darkorange",lwd=2)
legend("topleft", bty="n", legend=c("partial","sample"),lwd=4, 
       col=c("darkorange","grey25"))
plot(fpgrid, fpfit$ZZ.mean,type="l",lwd=2, bty="n",
     xlab="for-probability", ylab="log(salary in millions)",
     ylim=c(-.5,1.5), col="grey25")
lines(fpgrid, probfit$ZZ.mean,col="darkorange",lwd=2)
dev.off()














par(mfrow=c(1,2))
plot(pmgrid, pmfit$ZZ.mean,type="l",ylim=c(-1.5,3),lwd=2)
polygon(c(pmgrid,rev(pmgrid)), c(pmfit$ZZ.q1,rev(pmfit$ZZ.q2)), col=rgb(0,0,0,.1),border=FALSE)
plot(pmgrid, ppmfit$ZZ.mean,type="l",ylim=c(-1.5,3),lwd=2)
polygon(c(pmgrid,rev(pmgrid)), c(ppmfit$ZZ.q1,rev(ppmfit$ZZ.q2)), col=rgb(0,0,0,.1),border=FALSE)

var <- "pm"
s <- "20082009"
plot(log(sal) ~ pcut, data=nhldf, subset=which(nhldf$season==s))
## report coefficients on the pm and ppm statistics
fulllm <- lm(log(sal) ~ pm.corsi, data=nhldf)
coef(fulllm)[1:2]
