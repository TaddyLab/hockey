#Player performance analysis


To install the lattest [gamlr](https://github.com/mataddy/gamlr) package, you need to download the package from Github and wirte the following lines in R. 

	require(devtools)
	install("../gamlr-master")


The gamma lasso regularized model is applied to analyze players' performances. For the goals model (using the goals information), the influence of the plus-minus metric for players and special team effect on the ability of making goals, is analyzed. Correspondingly, similar anlysis is conducted by using shots information. **'gamlr_shots.R'** is the code to generate all results of the analyses. The detailed summary and comparition of the two models as well as the illustration of using 'gamlr' regression, can be found in the report 
**'compare_shots_goals.pdf'**. 

The above analyses focus on the career effect of players. The seasonal player effects (regression parameters of onice players) under both the goals model and the shots model, are generated for further analysis which can be found in files **'playereffect_goal.txt'** and **'playereffect_shot.txt'**. The code **'playereffect.R'** tells the procedure of generating the player effects in detail.




	
	
