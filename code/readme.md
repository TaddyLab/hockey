###### Copyright (C) 2015, Matt Taddy and Robert B Gramacy and Sen Tian

This document explains how to reproduce the results in section 4 (analysis) of the CRC paper using the code in this repository. More specifically, label '<...>' refers to the corresponding metric in the paper. For example, \<Table 1\> corresponds to Table 1 in the paper.

### Data Folder
**firstteam.txt**: the main team a player played for in each season.

**nhldesign-goal.rda**: event data based on goals.

**nhlsalaries.txt**: salary data obtained through <http://blackhawkzone.com> and <http://hockeyzoneplus.com>.

**roster.RData** roster information. 

**team_inv_rate.txt** the teams a player played for in each season, represented as weights. e.g, PAVEL_TRNKA spent half of 2002-2003 season in ANA (weight=0.5) while the other half in FLA (weight=0.5).

### Results Folder
**performance-\*.csv**: different metrics of player's performance based on the logistic regression \<Equation 1\>. 

**playoff-beta-\*.csv**: non-zero $$$\beta_p$$$ in \<Model 1\>, which indicates player's performance in the play-off seaon differs from regular.

**perfsaldf.rds**: aggregated salary and performance data by player.

### Code Folder
#### Preliminaries
**design.R** builds the basic event data for the analysis based on the NHL game data scraped using the R package **nhlscrapr** (available in CRAN). The output will be **nhldesign-\*.rda** where * depends on the flag you choose (GOAL, CORSI or FENWICK). 

#### Analysis
**performance.R** fits the logistic model \<Equation 1\> on the event data, and outputs a table of metrics \<Table 1,2&3\> including PM, PPM, Beta and etc. The table is stored in the *'results'* folder, named as **performance-\*.csv**. Meanwhile, non-zero $$$\beta_p$$$ in \<Model 1\>, which indicates player's performance in the play-off seaon differs from regular, can be found in *'results'* folder, named as **playoff-beta-\*.csv**.

**salary-build.R** aggregates the salary and performance by player to a single file **perfsaldf.rds**.

**salary-analysis.R** applies the nonparametric Bayesian regression to fit expected log salary on the performance, \<Figure 4\>. 

**table-plot.R** provides the details to obtain \<Table 2&4, Figure 3\>.


