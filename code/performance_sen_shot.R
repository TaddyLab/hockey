# Compare CORSI/FENWICK with Goals
###### Sen 
library(gamlr)
setwd("/Users/Sen/Documents/hockey_new")
# read the performance of each player over entire 11 seasons
perf_ent <- read.csv("results/performance-overall.csv",stringsAsFactors = FALSE)
# correlations
cor(perf_ent$beta.goals,perf_ent$beta.corsi)
cor(perf_ent$beta.goals,perf_ent$beta.fenwick)
cor(perf_ent$beta.corsi,perf_ent$beta.fenwick)
cor(perf_ent$ppm.goals,perf_ent$ppm.corsi)
cor(perf_ent$ppm.goals,perf_ent$ppm.fenwick)
cor(perf_ent$ppm.corsi,perf_ent$ppm.fenwick)

# plot ppm_corsi/fenwick vs ppm_goal
ind = (perf_ent$ppm.fenwick!=0 & perf_ent$ppm.goals!=0)
pos = perf_ent$position[ind]
color = c("black","red","green","blue","deepskyblue")[as.numeric(factor(pos))]
reg = lm(perf_ent$ppm.fenwick[ind]~perf_ent$ppm.goals[ind])
pdf(file="write-up/figures/fenwickvsgoals_overall.pdf", height=6*2, width=9*2)
plot(perf_ent$ppm.goals[ind],perf_ent$ppm.fenwick[ind],pch=pos,cex.lab=1.5,
     ylab="PPM_Fenwick",xlab="PPM_goal",col=color)
abline(reg,lwd=2)
abline(0,0,col='grey',lwd=2)
abline(v=0,col='grey',lwd=2)
dev.off()

# plot ppm_corsi vs ppm_fenwick
ind = (perf_ent$ppm.fenwick!=0 & perf_ent$ppm.corsi!=0)
pos = perf_ent$position[ind]
color = c("black","red","green","blue","deepskyblue")[as.numeric(factor(pos))]
reg = lm(perf_ent$ppm.fenwick[ind]~perf_ent$ppm.corsi[ind])
pdf(file="write-up/figures/fenwickvscorsi_overall.pdf", height=6*2, width=9*2)
plot(perf_ent$ppm.corsi[ind],perf_ent$ppm.fenwick[ind],pch=pos,cex.lab=1.5,
     ylab="PPM_Fenwick",xlab="PPM_Corsi",col=color)
abline(reg,lwd=2)
abline(0,0,col='grey',lwd=2)
abline(v=0,col='grey',lwd=2)
dev.off()

#### seasonal correlations
# read seasonal performances
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
                   ppm.goals=perf_goals[common,]$ppm.avg, ppm.corsi=perf_corsi[common,]$ppm.avg, 
                   ppm.fenwick=perf_fenwick[common,]$ppm.avg, row.names=common)
# seasons
seasons <- levels(factor(perf_goals$season))

corr_fengoal <- c()
corr_fencorsi <- c()
for(s in seasons){
  perf_s = perf[perf$season==s,]
  corr_fengoal <- c(corr_fengoal, cor(perf_s$ppm.fenwick,perf_s$ppm.goals))
  corr_fencorsi <- c(corr_fencorsi, cor(perf_s$ppm.fenwick,perf_s$ppm.corsi))
}
# make plot
pdf(file="write-up/figures/ppm-corr-diffmeasure-season.pdf", height=4*2, width=9*2)
plot(1:11,corr_fengoal,"l",lwd=2,ylab="Correlation ",ylim=c(0.2,1), xaxt="n", xlab="season",
     main="PPM measured via different measures",panel.first=abline(v=1:11,lty=1,col="grey"),
     cex.main=1.5)
lines(1:11,corr_fencorsi,lwd=2,col="red")
axis(1, at=1:11, labels=FALSE)
text(x=1:11, y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),
     labels=seasons,srt=45, adj=1, xpd=TRUE, cex=0.8, col=c("black","red")[(seasons=="20122013") + 1])
legend("bottomright", legend=c("measure in Fenwick vs goals", "measure in Fenwick vs Corsi"), col=c("black","red"), lwd=2, lty=rep(1,2),cex=1.5)
dev.off()

