load("nhlscrapr_logit_fits.RData")

## find the columns of XP indicating players active in seasion 20132014
XG <- as.data.frame(XG)
active <- rep(FALSE, ncol(XP))
for(i in 1:ncol(XP)) {
  if(max(XG$Season[XP[,i] != 0]) == 20132014)
    active[i] <- TRUE
}
not.active <- which(! active)

## players only
fitP$loadings <- as.matrix(fitP$loadings)
BP <- fitP$loadings[,-not.active,drop=FALSE]
colnames(BP) <- uN2[active]
BP <- sort(BP[,abs(BP)>0.0])

## players with special teams
fitSP$loadings <- as.matrix(fitSP$loadings)
BSP <- fitSP$loadings[,-c(1:7,not.active+7),drop=FALSE]
colnames(BSP) <- uN2[active]
BSP <- sort(BSP[,abs(BSP)>0.0])

## team and players
fitTP$loadings <- as.matrix(fitTP$loadings)
BTP = fitTP$loadings[,-c(1:31, not.active+31),drop=FALSE]
colnames(BTP) <- uN2[active]
BTP <- sort(BTP[,abs(BTP)>0.0])

## special teams, teams, and players
fitSTP$loadings <- as.matrix(fitSTP$loadings)
BSTP = fitSTP$loadings[,-c(1:38,not.active+38),drop=FALSE]
colnames(BSTP) <- uN2[active]
BSTP <- sort(BSTP[,abs(BSTP)>0.0])

## assemble the four fits into two data frames, one with Special teams
## and the other without (we will probably not use the one without)
both <- unique(c(names(BP), names(BSP), names(BTP), names(BSTP)))
B2 <- cbind(BP[match(both, names(BP))], BSP[match(both, names(BSP))],
            BTP[match(both, names(BTP))], BSTP[match(both, names(BSTP))])
dimnames(B2) <- list(both, model=c('P','SP','TP','STP'))
BS2 <- B2[,c(2,4)]
B2[is.na(B2)] <- 0
BS2[is.na(BS2)] <- 0
B2 <- B2[order(B2[,'STP']),]
BS2 <- BS2[order(BS2[,'STP']),]
B2 <- B2[apply(B2,1, function(b) max(abs(b))> 0),]
BS2 <- BS2[apply(BS2,1, function(b) max(abs(b))> 0),]

                      
## pdf("main_effects.pdf", width=14.5, height=6)
## plot a comparison between player-only and player-team models
## (both including special teams)
par(mai=c(1.5,1,.8,.4))
n <- nrow(BS2)
plot(1:n, BS2[,'STP'], col=0, xaxt='n', ylab='beta', xlab='',
     main="Coefficient given Team; tail links to unconditional estimates.", font.main=3,
     xlim=c(9,n-8))
nms <- rownames(BS2)
text(1:n, par("usr")[3]-.05, labels=nms, srt = 45, adj=1, xpd = TRUE, cex=.5)
axis(1, at=1:n, labels=rep("",n), las=2)
abline(h=0, col='Grey60')
for(i in 1:n){
  abline(v=i, col=8, lwd=.5)
  lines(x=c(i,i), y=BS2[i,], lwd=2, col=c('red','blue')[as.numeric(BS2[i,'SP']<BS2[i,'STP']) + 1]) 
}
points(1:n, BS2[,'STP'],  pch=21, bg='gray40', cex=0.3)
