#Chicago Hockey Analytics


This repository will contain analyses for the hockey players performance. This hub works as a supplementary analysis for the hockey project held by Prof. [Matt Taddy](http://faculty.chicagobooth.edu/matt.taddy/) and Prof. [Bobby Gramacy](http://faculty.chicagobooth.edu/robert.gramacy/). The main repository for the hockey project can be found at <https://github.com/mataddy/hockey>.

####Content

The data set I have used, containing all events within a single game for all games from season 2002 to the current season, is scraped from [nhl.com](https://nhl.com) by using [nhlscraper](https://github.com/acthomasca/nhlscrapr) tool from [A.C. Thomas](http://www.acthomas.ca/comment/). 

One of the main tools/methods is the regularized logistic regression, and more specifically the [Gamma Lasso Regression](https://github.com/mataddy/gamlr), which can be installed in R as the *'gamlr'* package.

I (Sen Tian) first pull relevent information (goals and shots information) from this data set and calculate the players' plus-minus values as well as special team effects. The details can be found in the **'General data sets'** folder. The data sets in this folder are shared throughout all the tasks.

The gamma lasso regularized model is applied to analyze players' performances. Both the onice players as well as special team effects are considered as factors that can influence the ability of making goals or shots. More details can be consulted to the **'Player performance'** folder.

The shot quality prediction is the next task. The influence of factors including 'distance of shots', 'session' and 'type of shots' on the ability of making goals, is analyzed by using a nonparametric ensemble method. More details can be refered to the **'Shot quality'** folder.

The second part of my work is to analyze whether the players are paid porperly. The salary data are obtained from BlackHawk Zone, which has salary history for each player in the NHL league starting from season 2002 to the current season.

I first analyze whether a team pays along the metric of the players' performances. The cleaned salary data set as well as details about the analysis can be found in the **'Player salary'** folder.



