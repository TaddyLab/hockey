#Chicago Hockey Analytics


This repository will contain analyses for the hockey players performance. This hub works as a supplementary analysis for the hockey project held by Prof. [Matt Taddy](http://faculty.chicagobooth.edu/matt.taddy/) and Prof. [Bobby Gramacy](http://faculty.chicagobooth.edu/robert.gramacy/). The main repository for the hockey project can be found at <https://github.com/mataddy/hockey>.

####Content

I (Sen Tian) start from conducting the hockey analysis by using **'hockey_shots.rda'** data, which is the output from **'buildshots.R'**. More details are provided in the README inside data or code branch. The main tool is the regularized logistic regression, and more specifically the [Gamma Lasso Regression](https://github.com/mataddy/gamlr), which is included in the *'gamlr'* package in R. The comparison of the results between using the 'goals' and 'shots' data sets is summarized in **'compare_shots_goals.pdf'** with R code **'gamlr_shots.R'**. 

The shot quality prediction is the second task. The analysis is mainly about the influence of factors 'distance of shots', 'session' and 'type of shots' on the goals making. The results are shown in **'shot_quality.pdf'** with R code **'shot_quality.R'**. 

I then apply the Gamma Lasso Regression for each season. The career performance as well as the performance by season are examined. The results are shown in **'playereffects.txt'** with R code **'playereffects.R'**.

The second part of my work is to analyze whether the players are paid porperly. The salary data are obtained from BlackHawk Zone, which has salary history for each player in the NHL league starting from season 2002 to the current season. 

The team involvement rate indicating the length of time the player has spent in this team over a specific season or his career, is calculated in **teaminvolve_rate.R** (the output is stored in **team_inv_rate.txt**). For example, the career team involvement matrix shows that the player AARON_GAGNON spent more than half of his career in DAL (rate/weight=0.5526316) and he spent the rest his career in WPG (rate/weight=0.4473684). I randomly picked 20 players to do sanity check of the team involvement rates. For each player, I compared the rates with the career path shown in nhl.com. The results proves the rationality of team involvement rates. 

