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
