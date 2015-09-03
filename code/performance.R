# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# regression analysis of hockey performance.
# The hockey data are built via design.R from games obtained via blog/scrape.R
# Salaries are taken from 'nhlsalaries_bhz_nc/txt' in the sentian/hockey fork

if(!exists("SHOTS")) SHOTS <- FALSE ## flag for including missed shots

## fitting the hockey gl model
library(gamlr)

## grab game data
if(SHOTS){ suffix <- "shots" 
} else{ suffix <- "goals" }
load(sprintf("data/nhldesign-%s.rda",suffix))

seasons <- levels(factor(goal$season))
who <- colnames(XP)

## Add player interactions with season and session
XPS <- Matrix(nrow=nrow(XP), ncol=0, sparse=TRUE)
for(s in seasons){
    print(s)
    # season-specific terms
    xps <- XP*(goal$season==s)
    colnames(xps) <- paste(who,s,sep="_")
    XPS <- cBind(XPS,xps)
    # and post-season effects
    xps <- xps*(goal$session=="Playoffs")
    colnames(xps) <- paste(colnames(xps),"Playoffs",sep="_")
    XPS <- cBind(XPS,xps)
}
XPS <- XPS[,colSums(XPS!=0)>0]
(dim(XPS))
## Add team-season-postseason interactions
xts <- XT*(goal$session=="Playoffs")
colnames(xts) <- paste(colnames(XT),"Playoffs",sep=".")
XT <- cBind(XT,xts)
XT <- XT[,colSums(XT!=0)>0]

#######  estimation
### per-season player effects
fit <- gamlr(cBind(XS,XT,XP,XPS), Y, gamma=0, 
	standardize=FALSE, verb=1,
	family="binomial", free=1:c(ncol(XS)+ncol(XT)))
B <- coef(fit)[-1,] # corrected AICc selection
#######

## find any players who's post-season differs from regular
postbeta <- B[grep("_Playoffs",names(B))]
(postbeta <- postbeta[postbeta!=0]) 
if(length(postbeta)>0){
    postbeta <- sort(postbeta,decreasing=TRUE)
    write.table(postbeta,
        file=sprintf("results/playoff-betadiff-%s.csv",suffix),
        sep=",", col.names=FALSE, quote=FALSE)
}
### tabulate metrics
# traditional plus minus
getpm <- function(now) colSums(XP[now,]*c(-1,1)[Y[now]+1]) 
# the pm "for percentage" (ie like corsi for percent)
getfp <- function(now){
    PM <- XP[now,]*c(-1,1)[Y[now]+1]
    F <- colSums(PM==1)
    A <- colSums(PM==-1)
    fp <- F/(F+A)
    fp[is.nan(fp)] <- 0
    fp
}
# total number of goals
getng <- function(now) colSums(abs(XP[now,])) 
# probability
getprob <- function(b){
    # The individual effect on probability that a
    # given goal is for vs against that player's team
    1/(1+exp(-b)) 
}
# partial plus minus
getppm <- function(p, ng){
    # multiply ng*p - ng*(1-p) to get expected plus-minus
    ng*(2*p-1)
}

perf <- data.frame()
for(s in seasons){
    print(s)
    # betas
    b <- B[who] + B[paste(who,s,sep="_")]
    b[is.na(b)] <- 0
    # regular season PMs and games
    now <- goal$season==s & goal$session=="Regular"
    pm <- getpm(now)
    fp <- getfp(now)
    ng <- getng(now)
    prob <- getprob(b)
    ppm <- getppm(prob,ng)
    # post-season info
    post <- goal$season==s & goal$session=="Playoffs"
    pmpost <- getpm(post)
    fppost <- getfp(post)
    ngpost <- getng(post)
    bpost <- (b + B[paste(names(b),s,"Playoffs",sep="_")])*(ngpost>0)
    bpost[is.na(bpost)] <- 0
    probpost <- getprob(bpost)
    ppmpost <- getppm(probpost,ngpost)
    # tabulate
    tab <- data.frame(
        player=who,
        season=s,
        beta=round(b,2),
        prob=round(probpost,2),
        ppm=round(ppm,2),
        pm=pm,
        fp=round(fp,2),
        beta.po=round(bpost,2),
        prob.po=round(probpost,2),
        ppm.po=round(ppmpost,2),
        pm.po=pmpost,
        fp.po=round(fppost,2)
        )
    rownames(tab) <- paste(tab$player, s, sep="_")
    # add to total
    perf <- rbind(perf, tab[ng>0,])
}
perf <- perf[order(-perf$ppm),] # rank
## write to file
rownames(perf) <- NULL
write.csv(perf, 
    file=sprintf("results/performance-%s.csv",suffix), 
    row.names=FALSE, quote=FALSE)
