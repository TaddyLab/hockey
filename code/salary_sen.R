# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# comparison between salary and performance
setwd("/Users/Sen/Documents/hockey_new")

# read performances
perf_goals <- read.csv("results/performance-goals.csv")
rownames(perf_goals) <- paste(perf_goals$player,perf_goals$season,sep="_")
perf_corsi <- read.csv("results/performance-corsi.csv")
rownames(perf_corsi) <- paste(perf_corsi$player,perf_corsi$season,sep="_")
perf_fenwick <- read.csv("results/performance-fenwick.csv")
rownames(perf_fenwick) <- paste(perf_fenwick$player,perf_fenwick$season,sep="_")

# common players
common <- intersect(intersect(rownames(perf_goals),rownames(perf_corsi)),rownames(perf_fenwick))
# build a new data frame
perf <- data.frame(player=perf_goals[common,]$player,season=perf_goals[common,]$season,
                   ppm.goals=perf_goals[common,]$ppm.avg, ppm.corsi=perf_corsi[common,]$ppm.avg, ppm.fenwick=perf_fenwick[common,]$ppm.avg, 
                   pm.goals=perf_goals[common,]$pm.avg, pm.corsi=perf_corsi[common,]$pm.avg, pm.fenwick=perf_fenwick[common,]$pm.avg,
                   row.names=common)

# seasons
seasons <- levels(factor(perf_goals$season))
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
## add salary to the data frame 
perf <- cbind(perf,salary=milperyear)

# write to file
rownames(perf) <- NULL
write.csv(perf, 
          file="results/performance-salary-todo.csv", 
          row.names=FALSE,quote=FALSE)

perf <- read.csv("results/performance-salary-todo.csv")
rownames(perf) <- paste(perf$player,perf$season,sep="_")
perf_s <- perf[perf$season=="20132014",]
ind <- which(perf_s$ppm.corsi==0|perf_s$ppm.goals==0)
# histogram of salary in season 20132014
pdf(file="write-up/figures/salaryhist.pdf", height=6, width=9)
hist(perf_s$salary,xlab="salary (USD, million)", xlim=c(0,max(perf_s$salary)),col="blue",cex.main=1.2,cex.lab=1.2,main="")
hist(perf_s$salary[ind],add=TRUE,col="white")
legend("topright", legend="full-data", col="grey", pch=15, cex=1.2, bty="n")
dev.off()


# remove 0 ppm
row_sub = apply(perf[,c("ppm.goals","ppm.corsi","ppm.fenwick")], 1, function(row) all(row!=0))
perf <- perf[row_sub,]

# scatterplot ppm vs salary, season 20132014
perf_s <- perf[perf$season=="20132014" & perf$salary>0,]
print(nrow(perf_s))
reg_goals <- lm(ppm.goals~salary,data=perf_s) # regression line
perf_s$ppm.corsi.rs <- perf_s$ppm.corsi*max(abs(perf_s$ppm.goals))/max(abs(perf_s$ppm.corsi)) # resacle ppm.corsi
reg_corsi <- lm(ppm.corsi.rs~salary,data=perf_s) # regression line

pdf(file="write-up/figures/ppmsal-scat.pdf", height=6*2, width=9*2)
par( mfrow = c( 1, 1 ),cex.axis=1.5,cex.lab=1.5,cex.main=2 )
plot(perf_s$salary,perf_s$ppm.goals,pch=1,xlab="salary (USD, million)",ylab="PPM",main="Season 2013-2014",
     panel.first=abline(v=perf_s$salary,lty=1,col="grey",lwd=0.5),cex.main=2,cex.lab=2)
abline(reg_goals,lwd=2)
points(perf_s$salary,perf_s$ppm.corsi.rs,pch=16,col="red")
abline(reg_corsi,col="red",lty=2,lwd=2)
legend("bottomright", legend=c("measure in goals","rescaled measure in Corsi"), col=c("black","red"), cex=2, pch=c(1,16))
dev.off()

# check the overpay and underpay players
perf_s[perf_s$salary>=8,c("ppm.goals","ppm.corsi.rs","salary")]

# read salary performance correlations
salcorr_goals <- read.csv("results/salarycorr-goals.csv", row.names=1)
salcorr_corsi <- read.csv("results/salarycorr-corsi.csv", row.names=1)
salcorr_fenwick <- read.csv("results/salarycorr-fenwick.csv", row.names=1)

# function to plot correlation btn salary and other statistics
plot.corr <- function(filename, solid, dashed, title, ymin=-0.2, ymax=0.5, x.legend=7, y.legend=-0.05){
  pdf(file=filename, height=6, width=9)
  plot(1:11,salcorr_goals[3:13,solid],"l",lwd=2,ylab="Correlation ",ylim=c(ymin,ymax), xaxt="n", xlab="",
       main=title,panel.first=abline(v=c(3,10),lty=1,col="grey"),cex.main=1.2,cex.lab=1.2)
  lines(1:11,salcorr_corsi[3:13,solid],lwd=2,col="red")
  lines(1:11,salcorr_fenwick[3:13,solid],lwd=2,col="blue")
  lines(1:11,salcorr_goals[3:13,dashed],lwd=2,col="black",lty=2)
  lines(1:11,salcorr_corsi[3:13,dashed],lwd=2,col="red",lty=2)
  lines(1:11,salcorr_fenwick[3:13,dashed],lwd=2,col="blue",lty=2)
  axis(1, at=1:11, labels=FALSE)
  text(x=1:11, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
       labels=seasons,srt=45, adj=1, xpd=TRUE, cex=1.2)
  legend(x.legend,y.legend, legend=c("goal-based", "Corsi-based", "Fenwick-based"), col=c("black","red","blue"), lwd=2, lty=rep(1,3),cex=1.2,bty="n")
  dev.off()
}

## 1. figure 1.3
# plot correlation btn ppm and salary over seasons
plot.corr(filename="write-up/figures/ppmpmsal-corr-season.pdf", solid="ppm.avg", dashed="pm.avg", 
          title="Correlation between salary and PPM (Solid), PM (Dashed)")
## 2. alternative to 1.3
## figure 1.4
# salary correlation with prob and FP
plot.corr(filename="write-up/figures/probfpsal-corr-season.pdf", solid="prob.avg", dashed="fp.avg", 
          title="Correlation between salary and prob (Solid), FP (Dashed)")
## figure 1.5
# salary correlation with positive effect
plot.corr(filename="write-up/figures/pos-ppmpmsal-corr-season.pdf", solid="pos.ppm.avg", dashed="pos.pm.avg", 
          title="Salary correlation only with positive effect")



