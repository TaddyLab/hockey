# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison between salary and performance

if(!exists("SHOTS")) SHOTS <- FALSE ## flag for including missed shots

## grab performance estimates
if(SHOTS){ suffix <- "shots" 
} else{ suffix <- "goals" }
perf <- read.csv(sprintf("results/performance-%s.csv",suffix))
rownames(perf) <- paste(perf$player,perf$season,sep="_")

seasons <- levels(factor(perf$season))
## read salary
salary <- read.table("data/nhlsalaries.txt", 
    sep="|", header=TRUE,quote="",as.is=TRUE)
salary$Name[salary$Name=="ANDREI_KASTSITSYN"] <- "ANDREI_KOSTITSYN"
salary$Name[salary$Name=="P. J._AXELSSON"] <- "P.J._AXELSSON"
colnames(salary) <- c("player",seasons,"total")

milperyear <- unlist(salary[,seasons])
names(milperyear) <- paste(salary$player, 
                        rep(seasons,each=nrow(salary)),
                        sep="_")
milperyear <- milperyear[match(rownames(perf),names(milperyear))]
names(milperyear) <- rownames(perf)

## who doesn't have salary? 
nosal <- which(is.na(milperyear))# | milperyear==0)
(length(nosal))
head(perf[nosal,])
## remove them from calculations
perf <- perf[-nosal,]
milperyear <- milperyear[-nosal]


## summarize correlations
salarycorr <- matrix(nrow=length(seasons)+1, ncol=6, 
    dimnames=list(c("overall",seasons),names(perf)[-(1:2)]))
salarycorr["overall",] <- cor(perf[,-(1:2)],milperyear)
for(s in seasons){
    ps <- perf$season==s
    salarycorr[s,] <- cor(perf[ps,-(1:2)],milperyear[ps])
}
(salarycorr <- round(salarycorr,3))
write.csv(salarycorr, 
    file=sprintf("results/salarycorr-%s.csv",suffix), 
    quote=FALSE)
