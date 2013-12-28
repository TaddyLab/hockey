library(gamlr)

## grab data and drop all the useless zeros
load("data/nhldesign.rda")

## design and penalization scheme
## team effects
# X <- cBind(XS,XT,XP)
# unpen <- 1:(ncol(XS)+ncol(XT))
## coach effects
X <- cBind(XS,XC,XP)
unpen <- 1:ncol(XS)

### career player effects
fit <- gamlr(X, Y, 
	gamma=10, standardize=FALSE,
	family="binomial", free=unpen)
B <- coef(fit, k=2)[-1,]
cat("nonzero career effects:", sum(B[colnames(XP)]!=0),"\n")

# ## pull out team effects
# BT <- sort(B[colnames(XT)],decreasing=TRUE)
# print(BT[BT!=0])
# write.table(BT,"results/team_effects.txt", quote=FALSE, col.names=FALSE, sep="|")

## pull out coach effects
BC <- sort(B[colnames(XC)],decreasing=TRUE)
names(BC) <- sub("COACH_","",names(BC))
print(BC[BC!=0])
write.table(BC,"results/coach_effects.txt", quote=FALSE, col.names=FALSE, sep="|")

### current season effects
thisseason<-"20132014"
now <- goal$season==thisseason
fit_now <- gamlr(X[now,colnames(XP)], Y[now], 
	fix=X[now,]%*%B, gamma=0, 
	standardize=FALSE, family="binomial")
Bdif <- coef(fit_now, k=2)[-1,]
cat("nonzero career effects:", sum(Bdif!=0),"\n")

## print
cbind(player[,c("position","active")],
	B=B[colnames(XP)],Bdif=Bdif)[Bdif!=0,]

## tabulate
Bcar <- B[colnames(XP)]
Bnow <- (Bcar + Bdif)*(player$active==thisseason)
tab <- data.frame(who=names(Bcar),
			last_active=player$active,
			career_effect=Bcar,
			current_effect=Bnow)
tab <- tab[order(-tab$career_effect,-tab$current_effect,tab$who),]
rownames(tab) <- 1:nrow(tab)
tab[1:25,]

write.table(tab, sep="|",
	file="results/player_effects.txt", row.names=FALSE, quote=FALSE)






