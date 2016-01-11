# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison between salary and performance

library(gamlr)

####################
## read performances
perf_goals <- read.csv("results/performance-goals.csv")
rownames(perf_goals) <- paste(perf_goals$player,perf_goals$season,sep="_")
perf_corsi <- read.csv("results/performance-corsi.csv")
rownames(perf_corsi) <- paste(perf_corsi$player,perf_corsi$season,sep="_")
perf_fenwick <- read.csv("results/performance-fenwick.csv")
rownames(perf_fenwick) <- paste(perf_fenwick$player,perf_fenwick$season,sep="_")

colnames(perf_corsi) <- paste(colnames(perf_corsi),"corsi",sep=".")
colnames(perf_fenwick) <- paste(colnames(perf_fenwick),"fenwick",sep=".")

perf <- perf_goals
perf <- cbind(perf, perf_corsi[match(rownames(perf),rownames(perf_corsi)),-(1:2)])
perf <- cbind(perf, perf_fenwick[match(rownames(perf),rownames(perf_fenwick)),-(1:2)])

seasons <- levels(factor(perf$season))

##############
## read salary
salary <- read.table("data/nhlsalaries.txt", 
    sep="|", header=TRUE,quote="",as.is=TRUE,row.names=1)
rownames(salary)[rownames(salary)=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
rownames(salary)[rownames(salary)=="P. J._AXELSSON"] <- "P.J._AXELSSON"
colnames(salary) <- c(seasons,"total")

sal <- unlist(salary[,seasons])
names(sal) <- paste(rownames(salary), 
                        rep(seasons,each=nrow(salary)),
                        sep="_")
sal <- sal[match(rownames(perf),names(sal))]
names(sal) <- rownames(perf)

## who doesn't have salary? 
nosal <- which(is.na(sal)| sal==0)
(length(nosal))
head(perf[nosal,])
## remove them from calculations
perf <- perf[-nosal,]
sal <- sal[-nosal]

################################
## read-in team involvement rate
tir <- read.table("data/team_inv_rate.txt",colClasses=c("factor","factor","numeric","factor"),
  header=TRUE,sep="|")
tir <- tir[tir$season!="career",]
tir$playerseason <- factor(paste(tir$player,tir$season,sep="_"))
majorteam <- sapply(
  levels(tir$playerseason), 
  function(w){
    tirw <- tir[tir$playerseason==w,]
    teamw <- tirw$team[which.max(tirw$weight)]
    return(teamw) })

team <- majorteam[match(rownames(perf),names(majorteam))]
names(team) <- rownames(perf)
noteam <- which(is.na(team))
team <- team[-noteam]
perf <- perf[-noteam,]
sal <- sal[-noteam]

#################################
## combine the data together
nhldf <- cbind(team,sal,perf)
saveRDS(nhldf, "results/perfsaldf.rds",compress=FALSE)

################################
## analysis
nhldf <- readRDS("results/perfsaldf.rds")
nhldf$season <- factor(nhldf$season)
plot(sal ~ season, data=nhldf)

## report coefficients on the pm and ppm statistics
fulllm <- lm(log(sal) ~ pm.corsi, data=nhldf)
coef(fulllm)[1:2]

###############################
## salary ratio stuff
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
        #sub("_"," ", tabpfp[i,"player"]),
        #tabpfp[i,"pfp.ratio"], 
        sub("_"," ", tabppm[i,"player"]),
        tab[i,"ppm.ratio"],
        round(tabppm[i,"ppm.ratio"],2),
          sep="&")
      cat("\\\\\n")
    }
 }
 printratio()


