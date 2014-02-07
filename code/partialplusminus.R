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

effect <- data.frame(beta=beta,ppm=ppm,pm=pm)

print(effect[order(-effect$ppm)[1:20],])