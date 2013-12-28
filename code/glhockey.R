library(gamlr)

## grab data and drop all the useless zeros
load("data/builtnhl.rda")

## design X
X <- cBind(XS,XC,XP)

## fit
fit <- gamlr(X, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:ncol(XS), verb=1)

## nonzero player effects
B <- coef(fit, k=2)[colnames(XP),]
sum(B!=0)

## build player table
tab <- data.frame(who=names(B),
			last_active=player$active,
			effect=B)[order(-B),]
rownames(tab) <- 1:nrow(tab)
tab[1:25,]

write.csv("results/player_effects.csv", 
	file=outfile, row.names=FALSE, quote=FALSE)

## pull out coaches
C <- sort(B[colnames(XC)],decreasing=TRUE)
names(C) <- sub("COACH_","",names(C))
write.table(C,"results/coach_effects.txt", quote=FALSE, col.names=FALSE, sep="|")








