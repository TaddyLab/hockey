Results folder
======

This folder contains the weekly-updated player ability summaries.

<ul>
	<li> logistic_map_betas.csv: sparse (MAP) point-estimates under the player-team and player-only models, via mnlm.  These
		are the results which are presented as <a href="http://faculty.chicago
		booth.edu/robert.gramacy/hockey/mapbetas_active_latest.html">bar-chart
		s</a> on our <a
		href="http://blogs.chicagobooth.edu/hockeyanalytics">blog</a>, except
		in that format players with zero-values are not shown.
	<li> logistic_mean_betas.csv: dense (posterior mean) estimates under the player-team and player-only models, via reglogit
	<li> logistic_pranks_betas.csv: posterior rankings and credible intervals under the player-team and player-only models, via reglogit
</ul>
