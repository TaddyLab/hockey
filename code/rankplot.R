rs <- read.csv("../results/logistic_pranks_betas.csv")

n <- nrow(rs)

prankfile <- paste("../results/prank_all_", 
	format(Sys.time(), "%Y%m%d"), ".pdf", sep="")
pdf(prankfile, height=200)
xrange <- range(rs[,-(1:3)])
par(mai=c(0,2,0.0,0.5), mgp=c(-1,0,0))
plot(rs$TPmed, ((n:1)+0.1), axes=FALSE, col="red", 
	 ylab="", cex=0.5, xlim=xrange, xlab="player rank")
segments(rs$TPq10, ((n:1)+0.1), rs$TPq90, ((n:1)+0.1), col="red")
points(rs$Pmed, ((n:1)-0.1), col="blue", cex=0.5)
segments(rs$Pq10, ((n:1)-0.1), rs$Pq90, ((n:1)-0.1), col="blue")
axis(2, at=(n:1), labels=rs$Player, tick=FALSE, las=2, cex.axis=0.5)
dev.off()