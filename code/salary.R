# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison of team and performance

library(gamlr)
perf <- read.csv("results/performance.csv")
salary <- read.table("data/nhlsalaries.txt", 
    sep="|", header=TRUE,quote="",as.is=TRUE)
salary$Name[salary$Name=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
salary$Name[salary$Name=="P. J._AXELSSON"] <- "P.J._AXELSSON"

rownames(perf) <- paste(perf$player, perf$season, sep="_")

seasons <- levels(factor(perf$season))
colnames(salary) <- c("player",seasons,"total")
salary <- salary[salary$player%in%perf$player,]
perf$salary <- rep(NA,nrow(perf))
for( s in seasons ){
    print(s)
    perf[paste(salary$player,s,sep="_"),"salary"] <- salary[,s]
}
perf <- perf[!is.na(perf$salary),]






# ## team presence material; not present up to 2013-2014
# team <- read.table("data/team_inv_rate.txt", sep="|", header=TRUE)
# team$playerseason <- paste(team$player, team$season, sep="_")
# team$playerseason <- factor(team$playerseason,levels=rownames(performance))
# team <- team[!is.na(team$playerseason),]
# xteam <- sparseMatrix(
#     i=as.numeric(team$playerseason),
#     j=as.numeric(team$team),
#     x=team$weight, 
#     dims=c(nrow(performance),nlevels(team$team)),
#     dimnames=list(levels(team$playerseason),levels(team$team)))