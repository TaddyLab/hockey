### simple `partial plus minus' calculations.

tab <- read.csv("results/gl_player_effects.csv",row.names=1)
beta <- tab[order(-tab$current_effect),3]
names(beta) <- rownames(tab)

library(Matrix)
load("data/nhldesign.rda")
XPN <- XP[goal$season=="20132014",names(beta)]

pm <- colSums(XPN)
ng <- colSums(abs(XPN))
p <- 1/(1+exp(-beta))
ppm <- ng*(2*p-1)

effect <- data.frame(player=names(beta),
	beta=round(beta,3),ppm=round(ppm,3),pm=pm)
effect <- effect[ng>0,] # on ice for goal this season
effect <- effect[order(-effect$ppm),]
rownames(effect) <- 1:nrow(effect)

print(effect[1:20,])

write.table(effect,file="results/current_season_ppm.csv",sep=",",row.names=FALSE)
