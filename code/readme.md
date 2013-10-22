#Code folder


To install the lattest [gamlr](https://github.com/mataddy/gamlr) package, you need to download the package from Github and wirte the following lines in R. 

	require(devtools)
	install("../gamlr-master")

This folder contains all the coding files. 

**'build_shots.R'** generates the shots information of each game, **'hockey_shots.rda'**. 

**'gamlr_shots.R'** is the code for generating results in the report, **'compare_shots_goals.pdf'** (in the results foler). 

**'playereffect.R'** generates a matrix of regression parameters, **'playereffect.txt'** (in the results foler). The parameters are obtained by applying 'gamlr' regression for the entire career as well as each individual season.

**'shot_quality.R'** is the code for generating results in the report, **'shot_quality.pdf'** (in the results foler). 


	
	
