library(gamlr)

## grab data and drop all the useless zeros
load("data/nhlscrapr_logit_data.RData")
XP <- as(XP,"dgCMatrix")
XC <- readRDS("data/coaches.rds")
n <- length(Y)

## record last active season
stopifnot(all(colSums(XP!=0)!=0))
active <- XG[,'Season'][XP@i[tail(XP@p,-1)] + 1]
who <- data.frame(uN2, active) 

## design matrix
X <- cBind(XS,XP) #cBind(XS,XC,XP)

## fit
fit <- gamlr(X, Y, gamma=10, standardize=FALSE,
	family="binomial", free=1:ncol(XS), verb=1)

## pull out coaches
C <- coef(fit, k=2)[(1+ncol(XS))+(1:ncol(XC)),]
write.table(C,"results/coach_effects.txt",
	quote=FALSE, col.names=FALSE,sep="|")
## number of nonzero player effects
B <- coef(fit, k=2)[-(1:(ncol(XS)+1)),] #1:(ncol(XS)+ncol(XC)) + 1),]
print(sum(B!=0))
## output in player table
tab <- cbind(who,B)[order(-B),]
names(tab) <- c("Who", "Last Active Year", "effect")
tab$Who <- as.character(tab$Who)
tab[1:20,]


outfile <- "results/logistic_map_betas.csv"
write.csv(tab, file=outfile, row.names=FALSE, quote=FALSE)










