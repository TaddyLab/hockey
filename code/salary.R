# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison between salary and performance

setwd("/Users/Sen/Documents/hockey_new")
# flags for CORSI/FENWICK/GOAL
CORSI = FALSE
FENWICK = FALSE
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


printsal <- function(metric,xlab){
  s <- which(perf$season=="20132014")
  tab <- perf[s,]

  linfit <- lm(log(milperyear[s]) ~ tab[,metric])
  tab$resid <- linfit$resid

  plot(tab[,metric], log(milperyear[s]),  
    xlab=xlab, ylab="log( salary in millions ) ", pch=21, bg=8)
  abline(a=coef(linfit)[1], b=coef(linfit)[2], col=2)

  tab <- tab[order(tab$resid),]

  for(i in 1:20){
    l <- nrow(tab)-(i-1)  
    cat( i,
      sub("_"," ", tab[i,"player"]),
      exp(tab[i,"resid"]), 
      l,
      sub("_"," ", tab[l,"player"]),
      exp(tab[l,"resid"]),
        sep="&")
    cat("\\\\\n")
  }
}

pdf("salaryscatter.pdf",width=3,height=8)
par(mfrow=c(1,1), mai=c(.9,.9,.2,.2))
#printsal("prob","goals-PFP")
printsal("ppm","goals-PPM")
dev.off()


printratio <- function(){
  s <- which(perf$season=="20132014")
  tab <- perf[s,]

  tab$pfp.ratio <- tab$prob/milperyear[s]
  tab$ppm.ratio <- tab$ppm/milperyear[s]

  tabpfp <- tab[order(-tab$pfp.ratio),]
  tabppm <- tab[order(-tab$ppm.ratio),]

  for(i in 1:20){
    cat( i,
      sub("_"," ", tabpfp[i,"player"]),
      tab[i,"pfp.ratio"], 
      sub("_"," ", tabppm[i,"player"]),
      tab[i,"ppm.ratio"],
        sep="&")
    cat("\\\\\n")
  }
}
printratio()
########## Sen added
# add the max(ppm,0) and max(pm,0)
salcorr.pos <- function(metric){
  ind = perf[,metric]>0
  salcorr.pos <- c(cor(perf[ind,metric],milperyear[ind]))
  players <- sub("\\_\\d+$","",names(milperyear[ind]))
  avgsal <- tapply(milperyear[ind],players,mean)
  perfavg <- tapply(perf[ind,metric],players,mean)
  salcorr.pos <- c(salcorr.pos, cor(perfavg,avgsal))
  for(s in seasons){
    ps <- perf$season==s & perf[,metric]>0
    salcorr.pos <- c(salcorr.pos, cor(perf[ps,metric], milperyear[ps]))
  }
  return(salcorr.pos)
}
# for ppm
salcorr.posppm.reg <- salcorr.pos("ppm") # regular
salcorr.posppm.avg <- salcorr.pos("ppm.avg") # overall
# for pm
salcorr.pospm.reg <- salcorr.pos("pm") # regular
salcorr.pospm.avg <- salcorr.pos("pm.avg") # overall

salarycorr <- cbind(salarycorr, pos.ppm=salcorr.posppm.reg, pos.pm=salcorr.pospm.reg,pos.ppm.avg=salcorr.posppm.avg, pos.pm.avg=salcorr.pospm.avg)
(salarycorr <- round(salarycorr,3))

write.csv(salarycorr, 
    file=sprintf("results/salarycorr-%s.csv",suffix), 
    quote=FALSE)
