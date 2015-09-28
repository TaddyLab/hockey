###### Goals
###### Sen 

setwd("/Users/Sen/Documents/hockey_new")
load("data/nhldesign-goals.rda")
library(gamlr)

# aggregate the seasons in XT
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

# model fit: team+player
fit_pt <- gamlr(cBind(XT,XP), Y, gamma=0, 
             standardize=FALSE, verb=1,
             family="binomial", free=1:ncol(XT))
B_pt <- coef(fit_pt)[-1,] # corrected AICc selection
# model fit: just player
fit_p <- gamlr(XP, Y, gamma=0, 
             standardize=FALSE, verb=1,
             family="binomial")
B_p <- coef(fit_p)[-1,] # corrected AICc selection
# model fit: team+player+special team
fit_pts <- gamlr(cBind(XS,XT,XP), Y, gamma=0, 
             standardize=FALSE, verb=1,
             family="binomial", free=1:c(ncol(XS)+ncol(XT)))
B_pts <- coef(fit_pts)[-1,] # corrected AICc selection

# coef of the players
B_pts.p = B_pts[(ncol(XS)+ncol(XT)+1):length(B_pts)]
B_pt.p = B_pt[(ncol(XT)+1):length(B_pt)]
B_p.p = B_p
# number of zero coefs
sum(B_pts.p==0)
sum(B_pt.p==0)
sum(B_p.p==0)

# dot and line plot for comparing models
plot.modelcomp <- function(ind, coef_dot, coef_line, title, filename, label.y="PPM", mentioned=NULL){
  pdf(file=filename, height=4*2, width=9*2)
  op <- par(mar=c(8,4,1,1))
  coef_dot = coef_dot[ind]
  coef_line = coef_line[ind]
  ind = order(coef_dot)
  plot(1:length(ind),coef_dot[ind],'p',pch=20,cex=1,
       main = title, cex.main=1.5, xaxt="n", xlab="",ylab=label.y,col='black',
       panel.first=abline(v=1:length(ind),lty=1,col="grey"),
       ylim=c(min(c(coef_dot,coef_line)),max(c(coef_dot,coef_line))))
  axis(1, at=1:length(ind), labels=FALSE)
  textcol = rep("black",length(ind))
  names(textcol) = names(coef_dot[ind])
  textcol[intersect(mentioned,names(textcol))] = "red"
  text(x=1:length(ind), y=par()$usr[3]-0.02*(par()$usr[4]-par()$usr[3]),
       labels=names(textcol),srt=45, adj=1, xpd=TRUE, cex=0.8, col=textcol)
  for(i in 1:length(ind)){
    if(coef_dot[ind][i]>coef_line[ind][i]){
      segments(i,coef_dot[ind][i],i,coef_line[ind][i],col="blue",lwd=2)
    }
    else{
      segments(i,coef_dot[ind][i],i,coef_line[ind][i],col="red",lwd=2)
    }
  }
  abline(0,0,col='grey',lwd=2)
  par(op)
  dev.off()
}

# ppm
ng <- colSums(abs(XP)) 
prob <- 1/(1+exp(-B_pts.p)) 
ppm_pts.p <- ng*(2*prob-1)
prob <- 1/(1+exp(-B_pt.p)) 
ppm_pt.p <- ng*(2*prob-1)
prob <- 1/(1+exp(-B_p.p)) 
ppm_p.p <- ng*(2*prob-1)

## 1.begin ##################################################################
##### team-player model vs player-only model
##### top 20 and bottom 20 players in either model
ind.top.pt = order(ppm_pt.p)[c(1:20,(2439-19):2439)]
ind.top.p =  order(ppm_p.p)[c(1:20,(2439-19):2439)]
# make the plot
plot.modelcomp(unique(c(ind.top.pt,ind.top.p)), coef_dot = ppm_pt.p, coef_line = ppm_p.p, 
               title="Team-player model (dots) vs player-only model (connecting lines)",
               filename = "write-up/figures/ptvsp_tb20.pdf", label.y="PPM",
               mentioned=c("SIDNEY_CROSBY","MARIAN_HOSSA","PAVEL_DATSYUK","JOE_THORNTON","ALEX_OVECHKIN","HENRIK_SEDIN",
                           "JACK_JOHNSON","NICLAS_WALLIN","PATRICK_LALIME","NICLAS_HAVELID","JAY_BOUWMEESTER", "BRIAN_BOUCHER",
                           "HENRIK_LUNDQVIST","CHRIS_PHILLIPS","BRENDAN_WITT","JOSE_THEODORE","PETER_FORSBERG"))
## 1.end ####################################################################

## 2.begin ##################################################################
##### team-special team-player model vs team-player model
##### top 20 and bottom 20 players in either model
ind.top.pts = order(ppm_pts.p)[c(1:20,(2439-19):2439)]
ind.top.pt = order(ppm_pt.p)[c(1:20,(2439-19):2439)]
# make the plot
plot.modelcomp(unique(c(ind.top.pts,ind.top.pt)), coef_dot = ppm_pts.p, coef_line = ppm_pt.p, 
               title="Team-special team-player model (dots) vs team-player model (connecting lines)",
               filename = "write-up/figures/ptsvspt_tb20.pdf", label.y="PPM",
               mentioned=c("SIDNEY_CROSBY","MARIAN_HOSSA","PAVEL_DATSYUK","JOE_THORNTON","ALEX_OVECHKIN","HENRIK_SEDIN",
                           "JACK_JOHNSON","NICLAS_WALLIN","PATRICK_LALIME","NICLAS_HAVELID","JAY_BOUWMEESTER", "BRIAN_BOUCHER",
                           "HENRIK_LUNDQVIST","CHRIS_PHILLIPS","BRENDAN_WITT","JOSE_THEODORE","PETER_FORSBERG"))
## 2.end ####################################################################

## 3.begin ##################################################################
##### team-special team-player model
##### table of the top 5 and bottom 5 players in ppm
##### table of team ppm 

# traditional plus minus
pm <- colSums(XP*c(-1,1)[Y+1]) 
# the pm "for percentage" (ie like corsi for percent)
PM <- XP*c(-1,1)[Y+1]
F <- colSums(PM==1)
A <- colSums(PM==-1)
fp <- F/(F+A)
fp[is.nan(fp)] <- 0

# total number of goals
ng <- colSums(abs(XP)) 

# probability
prob <- 1/(1+exp(-B_pts.p)) 

# partial plus minus
# multiply ng*p - ng*(1-p) to get expected plus-minus
ppm <- ng*(2*prob-1)

tab <- data.frame(
  player=colnames(XP),
  pos=player$position,
  gp=ng,
  beta=round(B_pts.p,2),
  ppm=round(ppm,2),
  pm=pm,
  fp=round(fp,2)
)
rownames(tab) <- NULL
tab <- tab[order(-tab$ppm),] # rank
tab[c(1:10,(nrow(tab)-9):nrow(tab)),]

# team ppm 
pm.team = colSums(XT*c(-1,1)[Y+1]) 
ng.team = colSums(abs(XT))
B_t = B_pts[(ncol(XS)+1):(ncol(XS)+ncol(XT))]
prob.team = 1/(1+exp(-B_t)) 
ppm.team <- ng.team*(2*prob.team-1)

tab.team <- data.frame(
  team = colnames(XT),
  ppm = round(ppm.team,2),
  pm = pm.team
)
rownames(tab.team) <- NULL
tab.team <- tab.team[order(-tab.team$ppm),] # rank
tab.team
## 3.end ####################################################################

## 4.begin ##################################################################
##### team-special team-player model
# plot ppm vs pm
ind4 = (ppm>0 & pm<0 | ppm<0 & pm>0)
pos = player$position[ind4]
color = c("black","red","green","blue","deepskyblue")[as.numeric(factor(pos))]
pdf(file="write-up/figures/ppmvspm.pdf", height=4*2, width=9*2)
plot(pm[ind4],ppm[ind4],pch=pos,xlab="PM",ylab="PPM",col=color)
abline(0,0,col='grey',lwd=2)
abline(v=0,col='grey',lwd=2)
dev.off()
## 4.end ####################################################################


