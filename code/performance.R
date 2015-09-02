# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# regression analysis of hockey performance.
# The hockey data are built via design.R from games obtained via blog/scrape.R
# Salaries are taken from 'nhlsalaries_bhz_nc/txt' in the sentian/hockey fork

## fitting the hockey gl model
library(gamlr)

## grab game data
load("data/nhldesign-goals.rda")

## designs
XPseason <- Matrix(nrow=nrow(XP), ncol=0, sparse=TRUE)
seasons <- levels(factor(goal$season))
for(s in seasons){
    print(s)
    xps <- XP*(goal$season==s)
    colnames(xps) <- paste(colnames(XP),s,sep="_")
    XPseason <- cBind(XPseason,xps)
}

### per-season player effects
fit <- gamlr(cBind(XS,XT,XP,XPseason), Y, gamma=0, 
	standardize=FALSE, verb=1,
	family="binomial", free=1:c(ncol(XS)+ncol(XT)))
B <- coef(fit)[-1,]

### tabulate metrics
perf <- data.frame(player=character(),
        season=character(),
        beta=numeric(),
        ppm=numeric(),
        pm=numeric())
who <- colnames(XP)
for(s in unique(goal$season)){
    print(s)
    now <- goal$season==s
    b <- B[who] + # betas
        B[paste(who,s,sep="_")]
    pm <- colSums(XP[now,]*c(-1,1)[Y[now]+1]) # traditional plus minus
    ng <- colSums(abs(XP[now,])) # total number of goals
    # The individual effect on probability that a
    # given goal is for vs against that player's team
    p <- 1/(1+exp(-b)) 
    # multiply ng*p - ng*(1-p) to get expected plus-minus
    ppm <- ng*(2*p-1)
    tab <- data.frame(player=who,season=s,
            beta=round(b,2),ppm=round(ppm,2),pm=pm)
    rownames(tab) <- paste(tab$player, tab$season, sep="_")
    perf <- rbind(perf, tab[ng>0,])
}
perf <- perf[order(-perf$ppm),] # rank

## read salary
salary <- read.table("data/nhlsalaries.txt", 
    sep="|", header=TRUE,quote="",as.is=TRUE)
salary$Name[salary$Name=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
salary$Name[salary$Name=="P. J._AXELSSON"] <- "P.J._AXELSSON"
colnames(salary) <- c("player",seasons,"total")

## match with performance
perf$salary <- rep(NA,nrow(perf))
for( s in seasons ){
    print(s)
    who <- paste(salary$player,s,sep="_")
    zebra <- match(who,rownames(perf))
    perf[zebra[!is.na(zebra)],"salary"] <- salary[!is.na(zebra),s]
}
perf <- perf[!is.na(perf$salary),]
rownames(perf) <- NULL

## write to file
write.csv(perf, 
    file="results/performance.csv", 
    row.names=FALSE, quote=FALSE)

## summarize correlations
money <- matrix(nrow=length(seasons)+1, ncol=3, 
    dimnames=list(c("overall",seasons),names(perf)[3:5]))
money["overall",] <- cor(perf[,3:5],perf$salary)
for(s in seasons){
    ps <- perf[perf$season==s,]
    money[s,] <- (cor(ps[,3:5],ps$salary))
}