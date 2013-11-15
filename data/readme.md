#Data folder


This folder contains all the data files. The files are updated per week. 

The **'roster.RData'** file contains all the roster information about the NHL games from season 2002-2003 to the current season. 

A data set containing all information of each game play is obtained by using [nulscraper](https://github.com/acthomasca/nhlscrapr) tool from [A.C.Thomas](http://www.acthomas.ca/comment/) to scrape [nhl.com](https://nhl.com). build_shots.R (in the code folder) is used to generate shots information of each game (**'hockey_shots.rda'**) from the data set. 

The seasonal salary data sets for each player from season 2002 until the current season is obtained from BlackHawk Zone. The data sets are cleaned and merged to a single set (**'nhlsalaries_bhz.txt'**). Also the correction of players' names by matching with the ones in the roster data set is conducted, **'nhlsalaries_bhz_nc.txt'**.

**'nhlsalaries_hzp.txt'** is another salary data set obtained from Hockeyzone Plus. The two salary data sets both have some missing values. But in general, the data from BlackHawk Zone is better to use. More comparisons of these two data sets are summarized in **'comparison_salarydata.md'**. (Use them at your own risk)
