library(gamlr)

load("../data/nhlscrapr_logit_data.RData")

XSP <- as(cbind(XS,XP),"dgCMatrix")
XTSP <- cBind(XT,XSP)

fitSP <- gamlr(XSP, Y, gamma=1, 
	family="binomial", free=1:ncol(XS), verb=1)
fitTSP <- gamlr(XTSP, Y, gamma=1, 
	family="binomial", free=1:(ncol(XS)+ncol(XT)), verb=1)

## plot
png("glpaths.png", width=9, height=4, units="in", res=300)
par(mfrow=c(1,2))
plot(fitTSP, col=NULL)
mtext("team effects", font=2, line=2, cex=1.2)
plot(fitSP, col=NULL)
mtext("player only", font=2, line=2, cex=1.2)
dev.off()

## coef
BSP <- coef(fitSP,k=2)[-c(1,fitSP$free+1),]
BTSP <- coef(fitTSP,k=2)[-c(1,fitTSP$free+1),]
## number of nonzero team-model effects
sum(BTSP!=0)
## number of nonzero player-only effects
sum(BSP!=0)

## output in player table
XG <- as.data.frame(XG)
active <- rep(NA, ncol(XP))
for(i in 1:ncol(XP)) active[i] <- max(XG$Season[XP[,i] != 0])

tab <- cbind(data.frame(uN2, active),BTSP,BSP)
names(tab) <- c("Player", "Last Active Year", 
	"Player-Team Model", "Player-Only Model")
tab$Player <- as.character(tab$Player)
outfile <- "../results/glhockey_betas.csv"
write.csv(tab[order(-tab[,3], -tab[,4]),], 
	file=outfile, row.names=FALSE, quote=FALSE)










