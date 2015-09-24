# Compare CORSI/FENWICK with Goals
###### Sen 
library(gamlr)
setwd("/Users/Sen/Documents/hockey_new")

B_pts.p <- list()
suffix <- c("goals","corsi","fenwick")
for(i in 1:3){
  load(sprintf("data/nhldesign-%s.rda",suffix[i]))
  XT.season = XT
  teams <- sort(unique(c(goal$hometeam,goal$awayteam)))
  XT = Matrix(0,ncol=length(teams),nrow=nrow(XT.season),dimnames=list(goal$gcode,teams))
  tc <- c(goal$hometeam,goal$awayteam)
  tf <- factor(tc,levels=teams)
  tw <- matrix(as.numeric(tf),ncol=2)
  XT[cbind(1:nrow(XT.season),tw[,1])] <- 1
  XT[cbind(1:nrow(XT.season),tw[,2])] <- -1
  XT = cbind(XT,"ATL/WPG"=XT[,"ATL"]+XT[,"WPG"])
  XT = XT[,-c(2,30)]
  # model fit: team+player+special team
  fit_pts <- gamlr(cBind(XS,XT,XP), Y, gamma=0, 
                   standardize=FALSE, verb=1,
                   family="binomial", free=1:c(ncol(XS)+ncol(XT)))
  B_pts <- coef(fit_pts)[-1,] # corrected AICc selection
  # coef of the players
  B_pts.p[[i]] = B_pts[(ncol(XS)+ncol(XT)+1):length(B_pts)]
}
names(B_pts.p) = suffix
common = intersect(intersect(names(B_pts.p[[1]]),names(B_pts.p[[2]])),names(B_pts.p[[3]]))
B_pts.p[[1]] = B_pts.p[[1]][common]
B_pts.p[[2]] = B_pts.p[[2]][common]
B_pts.p[[3]] = B_pts.p[[3]][common]
player = player[rownames(player) %in% common, ]

# plot beta_corsi/fenwick vs beta_goal
ind = (B_pts.p[["fenwick"]]!=0 & B_pts.p[["goals"]]!=0)
pos = player$position[ind]
color = c("black","red","green","blue","deepskyblue")[as.numeric(factor(pos))]
pdf(file="write-up/figures/fenwickvsgoals.pdf", height=4*2, width=9*2)
plot(B_pts.p[["goals"]][ind],B_pts.p[["fenwick"]][ind],pch=pos,ylab="Beta_FENWICK",xlab="Beta_goal",col=color)
abline(0,0,col='grey',lwd=2)
abline(v=0,col='grey',lwd=2)
dev.off()
