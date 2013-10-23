#Data folder


This folder contains all the data files. The files are updated per week. 

The **'roster.RData'** file contains all the roster information about the NHL games from season 2002-2003 to the current season. 

A data set containing all information of each game play is obtained by using [nulscraper](https://github.com/acthomasca/nhlscrapr) tool from [A.C.Thomas](http://www.acthomas.ca/comment/) to scrape [nhl.com](https://nhl.com). build_shots.R (in the code folder) is used to generate shots information of each game (**'hockey_shots.rda'**) from the data set. 

The seasonal salary data sets for each player from season 2002 until the current season is obtained from BlackHawk Zone. The data sets are cleaned and merged to a single set (**'nhlsalaries_bhz.txt'**).

Player effects, including the plus.minus value and position, are added to the salary data set (**'salary_pm.txt'**). 
