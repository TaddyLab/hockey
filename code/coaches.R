library(gamlr)

## grab data and drop all the useless zeros
load("data/nhlscrapr_logit_data.RData")
n <- length(Y)

## coach effects
coach <- as.matrix(read.csv("data/coach.csv",
	check.names=FALSE,row.names=1,as.is=TRUE))

## clean coaches table
coach[coach==""] <- "."
## RN 2 games @bench to reach 1k mark
coach[coach=="Roger Neilson, Jacques Martin"] <- "Jacques Martin"
## BS was let go in june
coach[coach=="Brad Shaw, Ted Nolan"] <- "Ted Nolan"
## AA 2 games @bench to reach 1k mark
coach[coach=="Al Arbour, Ted Nolan"] <- "Ted Nolan"
## less clear; JT coached 27 games after RD was fired
coach[coach=="Rick Dudley, John Torchetti"] <- "Rick Dudley"
## GG coached 82 games in 05-06...
coach[coach=="Gerard Gallant, Gary Agnew"] <- "Gerard Gallant"
## ... only 45 in 03-04 but was assistant to interm/gm DM for rest
coach[coach=="Doug MacLean, Gerard Gallant"] <- "Gerard Gallant"

saveRDS(coach,file="data/coach.rds")

## create coach effects
hc <- coach[cbind(goals$hometeam,goals$season)]
ac <- coach[cbind(goals$awayteam,goals$season)]

## who is the missing washington coach?
cf <- relevel(factor(c(hc,ac),levels=unique(c(hc,ac))),".")
p <- nlevels(cf)-1
cw <- matrix(as.numeric(cf)-1,ncol=2)
## turn binary
XC <- Matrix(0,nrow=n,ncol=p,
	dimnames=list(goals$gcode,levels(cf)[-1]))
XC[cbind(1:n,cw[,1])] <- 1
XC[cbind(1:n,cw[,2])] <- -1

saveRDS(XC,file="data/coaches.rds")
