# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison between salary and performance

setwd("/Users/Sen/Documents/hockey_new")
# flags for CORSI/FENWICK/GOAL
CORSI = FALSE
FENWICK = TRUE
if(CORSI & FENWICK){stop("Multiple flags")}

## grab performance estimates
if(CORSI){ suffix <- "corsi" 
} else if(FENWICK){ suffix <- "fenwick" 
} else{ suffix <- "goals"}
perf <- read.csv(sprintf("results/performance-%s.csv",suffix))
rownames(perf) <- paste(perf$player,perf$season,sep="_")

seasons <- levels(factor(perf$season))
## read salary
salary <- read.table("data/nhlsalaries.txt", 
    sep="|", header=TRUE,quote="",as.is=TRUE,row.names=1)
rownames(salary)[rownames(salary)=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
rownames(salary)[rownames(salary)=="P. J._AXELSSON"] <- "P.J._AXELSSON"
colnames(salary) <- c(seasons,"total")

milperyear <- unlist(salary[,seasons])
names(milperyear) <- paste(rownames(salary), 
                        rep(seasons,each=nrow(salary)),
                        sep="_")
milperyear <- milperyear[match(rownames(perf),names(milperyear))]
names(milperyear) <- rownames(perf)

## who doesn't have salary? 
nosal <- which(is.na(milperyear)| milperyear==0)
(length(nosal))
head(perf[nosal,])
## remove them from calculations
perf <- perf[-nosal,]
milperyear <- milperyear[-nosal]

## summarize correlations
salarycorr <- matrix(nrow=length(seasons)+2, ncol=ncol(perf)-2, 
    dimnames=list(c("overall","averaged",seasons),names(perf)[-(1:2)]))
# overall
salarycorr["overall",] <- cor(perf[,-(1:2)],milperyear)
# per-season
for(s in seasons){
    ps <- perf$season==s
    salarycorr[s,] <- cor(perf[ps,-(1:2)],milperyear[ps])
}
# average salary vs avg perf
players <- sub("\\_\\d+$","",names(milperyear))
avgsal <- tapply(milperyear,players,mean)
perfavg <- aggregate(perf[,-(1:2)],by=list(players),FUN=mean)
salarycorr["averaged",] <- cor(perfavg[,-1],avgsal)

# add the max(ppm,0) and max(pm,0)
# perf <- cbind(perf,max.ppm.0=pmax(perf$ppm.avg,0),max.pm.0=pmax(perf$pm.avg,0))
# for ppm
ind = perf$ppm.avg>0
salcorr.posppm <- c(cor(perf[ind,"ppm.avg"],milperyear[ind]))
players <- sub("\\_\\d+$","",names(milperyear[ind]))
avgsal <- tapply(milperyear[ind],players,mean)
perfavg <- tapply(perf[ind,"ppm.avg"],players,mean)
salcorr.posppm <- c(salcorr.posppm, cor(perfavg,avgsal))
for(s in seasons){
  ps <- perf$season==s & perf$ppm.avg>0
  salcorr.posppm <- c(salcorr.posppm, cor(perf[ps,"ppm.avg"], milperyear[ps]))
}
# for pm
ind = perf$pm.avg>0
salcorr.pospm <- c(cor(perf[ind,"pm.avg"],milperyear[ind]))
players <- sub("\\_\\d+$","",names(milperyear[ind]))
avgsal <- tapply(milperyear[ind],players,mean)
perfavg <- tapply(perf[ind,"pm.avg"],players,mean)
salcorr.pospm <- c(salcorr.pospm, cor(perfavg,avgsal))
for(s in seasons){
  ps <- perf$season==s & perf$pm.avg>0
  salcorr.pospm <- c(salcorr.pospm, cor(perf[ps,"pm.avg"], milperyear[ps]))
}

salarycorr <- cbind(salarycorr, pos.ppm.avg=salcorr.posppm, pos.pm.avg=salcorr.pospm)
(salarycorr <- round(salarycorr,3))

write.csv(salarycorr, 
    file=sprintf("results/salarycorr-%s.csv",suffix), 
    quote=FALSE)
