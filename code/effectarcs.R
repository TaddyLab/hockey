library(gamlr)

## grab data and drop all the useless zeros
load("data/builtnhl.rda")

## build time dynamics
delta <- 0.1
XT <- list()
pj <- summary(XP)$j
year <- as.numeric(substr(goal$season,5,8))
for(s in rev(unique(goal$season))[-1]){
  t <- as.numeric(substr(s,5,8))
  xs <- XP
  xs@x <- xs@x*delta*(year[xs@i+1]<=t)*(lastyear[pj]>t)
  colnames(xs) <- paste(colnames(xs),s,sep=".")
  XT[[s]] <- xs[,colSums(xs!=0)!=0]
  cat(t,"\n")
}
XT <- do.call(cBind,rev(XT))

# # HASEK test: he sat out 02-03 and retired in 08.
xtest <- XT[,grep("DOMINIK_HASEK",colnames(XT))]
xtest <- xtest[apply(xtest,1,function(r) any(r!=0)),]
xtest[c(1,100,400),]

## design X
X <- cBind(XS,XC,XP,XT)

## fit
fit <- gamlr(X, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:ncol(XS), verb=1)

## nonzero player effects in last active year
B <- coef(fit, k=2)[-1,]
BP <- B[colnames(XP)]
cat("nonzero mains:", sum(BP!=0), "\n")
BT <- B[colnames(XT)]
cat("nonzero deltas:", sum(BT!=0), "\n")

## build player table
p <- nrow(player)
stopifnot(all(rownames(player)==names(BP)))
seasons <- rev(unique(goal$season))
tab <- matrix(0, 
	nrow=p, ncol=length(seasons), 
	dimnames=list(names(BP),seasons))
tab[cbind(names(BP),player$active)] <- BP

for(t in 2:ncol(tab)){
	s <- seasons[t]
	tab[,t] <- tab[,t-1]*(player$entry<=s)
	bs <- BT[grep(s,names(BT))]
	bs <- bs[bs!=0]
	if(length(bs)>0){
		for(w in sub(sprintf(".%s",s),"",names(bs)))
			tab[w,seasons>=s] <- tab[w,seasons>=s]+bs[w]
	}
	cat("nonzero in",s,": ", length(bs), "\n")
}
tab <- tab[order(-tab[cbind(rownames(tab),player$active)]),]

tab[1:25,]

outfile <- "results/player_effects.csv"
write.csv(tab, file=outfile, row.names=FALSE, quote=FALSE)

## pull out coaches
C <- sort(B[colnames(XC)],decreasing=TRUE)
names(C) <- sub("COACH_","",names(C))
write.table(C,"results/coach_effects.txt", quote=FALSE, col.names=FALSE, sep="|")








