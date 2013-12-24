library(gamlr)

## grab data and drop all the useless zeros
load("data/nhlscrapr_logit_data.RData")
n <- length(Y)

## coach effects
coach <- as.matrix(read.csv("data/coach.csv",
	check.names=FALSE,row.names=1,as.is=TRUE))
# organize goals
stopifnot(all(goals$gcode==XG[,"GameCode"]))
goals$season <- as.character(goals$season)
goals$awayteam <- as.character(goals$awayteam)
goals$hometeam <- as.character(goals$hometeam)
goals$awayteam[goals$awayteam == "S.J"] <- "SJS"
goals$awayteam[goals$awayteam == "L.A"] <- "LAK"
goals$awayteam[goals$awayteam == "T.B"] <- "TBL"
goals$awayteam[goals$awayteam == "N.J"] <- "NJD"
goals$hometeam[goals$hometeam == "S.J"] <- "SJS"
goals$hometeam[goals$hometeam == "L.A"] <- "LAK"
goals$hometeam[goals$hometeam == "T.B"] <- "TBL"
goals$hometeam[goals$hometeam == "N.J"] <- "NJD"


## create coach effects
ac <- coach[cbind(goals$awayteam,goals$season)]
ac <- t(sapply(ac, function(s){
		s <- unlist(strsplit(s, ", "))
		if(length(s)==0) s <- c(".",".")
		if(length(s)==1) s <- c(s, ".")
		return(s)}))
hc <- coach[cbind(goals$hometeam,goals$season)]
hc <- t(sapply(hc, function(s){
		s <- unlist(strsplit(s, ", "))
		if(length(s)==0) s <- c(".",".")
		if(length(s)==1) s <- c(s, ".")
		return(s)}))

## who is the missing washington coach?
cf <- relevel(factor(c(hc,ac),levels=unique(c(hc,ac))),".")
p <- nlevels(cf)-1
cw <- matrix(as.numeric(cf)-1,ncol=4)
## turn binary
XC <- Matrix(0,nrow=n,ncol=p,
	dimnames=list(goals$gcode,levels(cf)[-1]))
XC[cbind(1:n,cw[,1])] <- 1
XC[cbind(1:n,cw[,2])] <- 1
XC[cbind(1:n,cw[,3])] <- -1
XC[cbind(1:n,cw[,4])] <- -1

saveRDS(XC,file="data/coaches.rds")
