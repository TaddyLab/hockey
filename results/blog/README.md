Results folder
======

This folder contains the weekly-updated player ability summaries.

<ul>
	<li> gl_player_effects.csv: sparse (MAP) point-estimates under the 
		two models incorporating player, team, and special teams data, via <a href="https://github.com/mataddy/gamlr">gamlr</a>.  The current effects model up-weights the current year 
		(by one unit) relative to the total effects model in order to highlight changes in player
		ability over the current season.  These
		are the results which are presented as 
		<a href="http://faculty.chicagobooth.edu/robert.gramacy/hockey/mapbetas_active_latest.html">bar-charts</a> on our 
		<a href="http://blogs.chicagobooth.edu/hockeyanalytics">blog</a>, except
		in that format players with zero-values are not shown.
	<li> gl_team_effects.csv: shows a similar analysis as above, highlighting the team contribution
		to goals scored.  The analysis is broken down by year.
	<li> rl_mean_effects.csv: dense (posterior mean) total effect estimates estimates under the player, team
	and special teams models, via <a href="http://cran.r-project.org/web/packages/reglogit/index.html">reglogit</a>
	<li> rl_pranks_effects.csv: posterior rankings and credible intervals extracted from the same posterior
		distribution as above
</ul>

The files below are outputs from legacy analysis, used before the development of the total v. current
year updates.
<ul>
	<li> logistic_map_betas.csv: sparse (MAP) point-estimates under the player-team and player-only models, via mnlm.  These
		<i>were</i> the results which are presented as 
		<a href="http://faculty.chicagobooth.edu/robert.gramacy/hockey/mapbetas_active_latest.html">bar-charts</a> on our 
		<a href="http://blogs.chicagobooth.edu/hockeyanalytics">blog</a>, except
		in that format players with zero-values are not shown.
	<li> logistic_mean_betas.csv: dense (posterior mean) estimates under the player-team and player-only models, via <a href="http://cran.r-project.org/web/packages/reglogit/index.html">reglogit</a>
	<li> logistic_pranks_betas.csv: posterior rankings and credible intervals under the player-team and player-only models, under the same posterior distribution as above
</ul>
