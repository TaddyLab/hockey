#Code folder


To install the lattest [gamlr](https://github.com/mataddy/gamlr) package, you need to download the package from Github and wirte the following lines in R. 

	require(devtools)
	install("../gamlr-master")

This folder contains all the coding files. 

**'build_shots.R'** generates the shots information of each game, **'hockey_shots.rda'**. 

**'gamlr_shots.R'** is the code for generating results in the report, **'compare_shots_goals.pdf'** (in the results foler). 

**'playereffect.R'** generates a matrix of regression parameters, **'playereffect.txt'** (in the results foler). The parameters are obtained by applying 'gamlr' regression for the entire career as well as each individual season.

**'shot_quality.R'** is the code for generating results in the report, **'shot_quality.pdf'** (in the results foler). 

**'clean_salary_bhz.R'** is the code for cleaning and merging seasonal salary data obtained from BlackHawk Zone. The merged salary data is given in **'nhlsalaries_bhz.txt'**. Also the player names in the salary data are corrected by matching with the names in the roster data, **'nhlsalaries_bhz_nc.txt'**. 

**'teaminvolve_rate.R'** calculates the team involvement rate (seasonal and career) for each player in the nhl league. We have 2412 players and 31 nhl teams (note: ATL was the former of WPG, we treat them separately). The outputs are eleven 2412*31 matrices (10 seasons and 1 career), **'team_inv_rate.txt'**. 


	
	
