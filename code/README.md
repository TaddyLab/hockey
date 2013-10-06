Code folder
======

This folder contains the scripts that are run regularly (weekly for now) to scrape data from the
web and clean it for analysis (saving the results in the data folder); run
several flavors of regularized logistic regression methods to estimate player
abilities; and format the output for the results folder.

They are executed in the following order:

<ul>
	<li> scrape.R: uses the nhlscrapr tool to update a games database stored locally (i.e., off-line)
	<li> buildgoals.R: generates goals data from the local database, storing the results in the data folder
	<li> logistic_fits.R: fits several flavors of regularized logistic regressions on the goals data,
		storing the output objects locally
	<li> logistic_textout.R: interprets the local output objects to build the text/csv files output to the results folder
</ul>

The other files are for development purposes only.
