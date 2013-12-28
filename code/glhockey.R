library(gamlr)

## grab data and drop all the useless zeros
load("data/builtnhl.rda")

## design X
X <- cBind(XS,XC,XP)

## fit
fit <- gamlr(X, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:ncol(XS), verb=1)

## number of nonzero player effects
B <- coef(fit, k=2)[colnames(XP),]
print(sum(B!=0))

## output in player table
stopifnot(all(rownames(player)==colnames(XP)))
tab <- data.frame(
	rownames(player),
	player$active, B)[order(-B),]
names(tab) <- c("Who", "Last Active Year", "effect")
tab[1:25,]

outfile <- "results/player_effects.csv"
write.csv(tab, file=outfile, row.names=FALSE, quote=FALSE)

## pull out coaches
C <- sort(coef(fit, k=2)[1+ncol(XS) + 1:ncol(XC),],decreasing=TRUE)
write.table(C,"results/coach_effects.txt", quote=FALSE, col.names=FALSE, sep="|")








