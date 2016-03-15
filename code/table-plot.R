# Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian
# Tables and plots in the CRC paper

# data
gtab <- read.csv("results/performance-goals.csv")
ctab <- read.csv("results/performance-corsi.csv")
nhldf <- readRDS("results/perfsaldf.rds")
nhldf$season <- factor(nhldf$season)

###############################
# top 20 player-seasons by goal and Corsi-based PFP
# Table 2 of crc paper
printline <- function(i, tab){
  cat( i,sub("_"," ", tab[i,"player"]),
    unlist(tab[i,c("season","prob","fp","ppm","pm",
   "prob.po","fp.po", "ppm.po", "pm.po")]), sep="&")
  cat("\\\\\n")
}
for(i in 1:25) printline(i,gtab)
for(i in nrow(gtab)-19:0) printline(i,gtab)

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

###############################
# histogram of salary for 11 seasons
# Figure 3 of crc paper
## corsi-based
pdf(file="write-up/figures/salaryhist-corsi-regular.pdf", height=6, width=9)
hist(nhldf$sal,xlab="salary (USD, million)", xlim=c(0,max(nhldf$sal)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(nhldf$sal[nhldf$ppm.corsi<=0],add=TRUE,col="green")
hist(nhldf$sal[nhldf$ppm.corsi<0],add=TRUE,col="red")
legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
       , col=c("blue","green","red"), pch=15, cex=2, bty="n")
dev.off()
## goal-based
pdf(file="write-up/figures/salaryhist-goals-regular.pdf", height=6, width=9)
hist(nhldf$sal,xlab="salary (USD, million)", xlim=c(0,max(nhldf$sal)),col="blue",cex.main=1.2,cex.lab=1.5,main="")
hist(nhldf$sal[nhldf$ppm<=0],add=TRUE,col="green")
hist(nhldf$sal[nhldf$ppm<0],add=TRUE,col="red")
#legend("topright", legend=c("players with positive player-effects","players with zero player-effects","players with negative player-effects")
#       , col=c("blue","green","red"), pch=15, cex=1.2, bty="n")
dev.off()


###############################
## salary ratio stuff
# Table 4 of crc paper
 printratio <- function(){
   s <- which(nhldf$season=="20132014")
   tab <- nhldf[s,]
 
   tab$pfp.ratio <- tab$prob/tab$sal
   tab$ppm.ratio <- tab$ppm/tab$sal
 
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
