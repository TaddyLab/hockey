#General data folder


This folder contains data files shared by all analyses. 

The **'roster.RData'** file contains all the roster information about the NHL games from season 2002-2003 to the current season. 

A data set containing all information of each game play is obtained by using [nulscraper](https://github.com/acthomasca/nhlscrapr) tool from [A.C.Thomas](http://www.acthomas.ca/comment/) to scrape [nhl.com](https://nhl.com). The code **build_goals.R** pulls out all the information about goals and stores in the file **'nhlscrapr_goals.rda'**. Also the code is used to calculate the plus-minus value for each player and special team effect according to the goals information, which can be found in file **'hockey_goals.rda'**. Similarly, files **'nhlscrapr_shots.rda'** and **'hockey_shots.rda'** are the outputs of code **build_shots.R** by using shots information.


