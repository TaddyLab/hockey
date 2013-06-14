
library(doMC)

registerDoMC(4)
EXT = "../data"

G <- read.table("../data/games.txt", 
 		sep="|", comment="", quote="")

res <- foreach (i=1:nrow(G)) %dopar% {
	gr <- read.table(sprintf("%s/%d-%d-gamerec.txt",
						EXT,G$season[i], G$gcode[i]), 
						sep="|", comment="", quote="")

	write.table( append=TRUE)

	if (i%%100 == 0) 
        message(paste("matrix assembly: game", i, "of", nrow(G)))
}
