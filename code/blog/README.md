Code folder
======

This folder contains the scripts that are run regularly (weekly for now) to scrape data from the
web and clean it for analysis (saving the results in the data folder); run
several flavors of regularized logistic regression methods to estimate player
abilities; and format the output for the results folder.

They are executed in the following order:

<ul>
	<li> scrape.R: uses the <a href="http://cran.r-project.org/package=nhlscrapr">nhlscrapr</a> tool to update a games database stored locally (i.e., off-line)
	<li> buildgoals.R: generates goals data from the local database, storing the results in the data folder
	<li> glfit.R: fits MAP estimators (using <a href="https://github.com/mataddy/gamlr">gamlr</a>) calculating 	total effects using player, team, and special teams data for all seasons (since 2001), and a second model 	hilighting current season effects; outputs are stored in results/gl_*effects.csv
	<li> rlfit.R: calculate the fully Bayesian posterior distribution (using <a href="http://cran.r-project.org/web/packages/reglogit/index.html">reglogit</a>)  of total player effects using
		player, team, and special teams data for all seasons (since 2001); mean and rank intervals are
		stored in results/rl_*effects.csv
	<li> glchart.R: converts the text/csv files output files from glfit.R into the
		<a href="http://faculty.chicagobooth.edu/robert.gramacy/hockey/mapbetas_active_latest.html">weekly charts</a>
		which are linked from the <a href="http://blogs.chicagobooth.edu/hockeyanalytics">blog</a>
</ul>

The other files are for development purposes only, or are legacy code used for earlier 
versions of our analysis.

The code is licensed under the <a href="http://www.gnu.org/copyleft/lesser.html">GNU Lesser Public License</a>.
