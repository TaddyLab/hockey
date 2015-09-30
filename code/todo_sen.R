gtab <- read.csv("results/performance-goals.csv")
ctab <- read.csv("results/performance-corsi.csv")

printline <- function(i, tab){
  cat( i,sub("_"," ", tab[i,"player"]),
    unlist(tab[i,c("season","prob","fp","ppm","pm",
   "prob.po","fp.po", "ppm.po", "pm.po")]), sep="&")
  cat("\\\\\n")
}
for(i in 1:25) printline(i,gtab)
for(i in nrow(tab)-19:0) printline(i,gtab)

octab <- ctab[order(-ctab$prob),]
ogtab <- gtab[order(-gtab$prob),]

printprob <- function(i){
  cat( i,
    sub("_"," ", ogtab[i,"player"]),
    unlist(ogtab[i,c("season","prob")]), 
    sub("_"," ", octab[i,"player"]),
    unlist(octab[i,c("season","prob")]),sep="&")
  cat("\\\\\n")
}
for(i in 1:20) printprob(i)





# read salary performance correlations
salcorr_goals <- read.csv("results/salarycorr-goals.csv", row.names=1)
salcorr_corsi <- read.csv("results/salarycorr-corsi.csv", row.names=1)
salcorr_fenwick <- read.csv("results/salarycorr-fenwick.csv", row.names=1)


seasons = rownames(salcorr_goals)[-c(1,2)]

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
       labels=seasons,srt=45, adj=1, xpd=TRUE, cex=1.2,col=c("black","red")[seasons %in% c("20052006","20122013") +1 ])
  legend(x.legend,y.legend, legend=c("goal-based", "Corsi-based", "Fenwick-based"), col=c("black","red","blue"), lwd=2, lty=rep(1,3),cex=1.2,bty="n")
  dev.off()
}

################## 1. figure 1.3
# plot correlation btn ppm and salary 
plot.corr(filename="write-up/figures/ppmpmsal-corr-regular.pdf", solid="ppm", dashed="pm", 
          title="Correlation between salary and PPM (Solid), PM (Dashed)")
################# 2. alternative to 1.3
## figure 1.4
# salary correlation with prob and FP
plot.corr(filename="write-up/figures/probfpsal-corr-regular.pdf", solid="prob", dashed="fp", 
          title="Correlation between salary and prob (Solid), FP (Dashed)")
## figure 1.5
# salary correlation with positive effect
plot.corr(filename="write-up/figures/pos-ppmpmsal-corr-regular.pdf", solid="pos.ppm", dashed="pos.pm", 
          title="Salary correlation only with positive effect")

################ 3. players over/under paid






################ 4. histogram of salary
perf <- read.csv("results/performance-salary-todo.csv")
rownames(perf) <- paste(perf$player,perf$season,sep="_")
perf_s <- perf[perf$season=="20132014",]
# histogram of salary in season 20132014
## corsi-based
pdf(file="write-up/figures/salaryhist-1314-corsi-regular.pdf", height=6, width=9)
hist(perf_s$salary,xlab="salary (USD, million)", xlim=c(0,max(perf_s$salary)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(perf_s$salary[perf_s$ppm.corsi<=0],add=TRUE,col="green")
hist(perf_s$salary[perf_s$ppm.corsi<0],add=TRUE,col="red")
legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
       , col=c("blue","green","red"), pch=15, cex=2, bty="n")
dev.off()
## goal-based
pdf(file="write-up/figures/salaryhist-1314-goals-regular.pdf", height=6, width=9)
hist(perf_s$salary,xlab="salary (USD, million)", xlim=c(0,max(perf_s$salary)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(perf_s$salary[perf_s$ppm.goals<=0],add=TRUE,col="green")
hist(perf_s$salary[perf_s$ppm.goals<0],add=TRUE,col="red")
#legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
#       , col=c("blue","green","red"), pch=15, cex=1.2, bty="n")
dev.off()


# histogram of salary for 11 seasons
perf_s <- perf
## corsi-based
pdf(file="write-up/figures/salaryhist-corsi-regular.pdf", height=6, width=9)
hist(perf_s$salary,xlab="salary (USD, million)", xlim=c(0,max(perf_s$salary)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(perf_s$salary[perf_s$ppm.corsi<=0],add=TRUE,col="green")
hist(perf_s$salary[perf_s$ppm.corsi<0],add=TRUE,col="red")
legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
       , col=c("blue","green","red"), pch=15, cex=2, bty="n")
dev.off()
## goal-based
pdf(file="write-up/figures/salaryhist-goals-regular.pdf", height=6, width=9)
hist(perf_s$salary,xlab="salary (USD, million)", xlim=c(0,max(perf_s$salary)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(perf_s$salary[perf_s$ppm.goals<=0],add=TRUE,col="green")
hist(perf_s$salary[perf_s$ppm.goals<0],add=TRUE,col="red")
#legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
#       , col=c("blue","green","red"), pch=15, cex=1.2, bty="n")
dev.off()
