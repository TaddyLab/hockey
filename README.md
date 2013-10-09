Chicago Hockey Analytics
======

This repository will contain a set of analyses of player ability in the NHL.
It is designed to be a hub for experimentation, demonstration, and teaching
about both data mining and hockey analysis.  It also serves as the back-end
for the results published weekly on our <a
href="http://blogs.chicagobooth.edu/hockeyanalytics">blog</a>.  Note those are
a subset of the full suite provided by the codes stored here.

<strong> Background </strong>

The models here are all descendant from an academic <a href="http://www.degruy
ter.com/view/j/jqas.2013.9.issue-1/jqas-2012-0001/jqas-2012-0001.xml?format=IN
T">paper</a> (<a href="http://arxiv.org/abs/1209.5026">arXiv version</a>) that
uses relatively new techniques to estimate hockey player performance.  Written
by <a href="http://faculty.chicagobooth.edu/robert.gramacy">Robert B.
Gramacy</a>, <a href="http://www-stat.wharton.upenn.edu/~stjensen">Shane
Jensen</a>, and <a href="http://faculty.chicagobooth.edu/matt.tadd">Matt
Taddy</a>, it applies regularized logistic regression to predict the
probability that either team on the ice scored any given goal.  The
contribution of individual players to this for-vs-against goal probability is
then taken as a measurement of their on-ice contribution.  It is a sort of
regression-adjusted Plus-Minus.

<strong> Content </strong>

The repository contains R code to first pull the relevant data from nhl.com
(using the <a href="https://github.com/acthomasca/nhlscrapr">nhlscrapar</a>
tool from <a href="http://www.acthomas.ca/comment/">A.C. Thomas</a>), and then
do a series of analyses using sparse regularized regression. Details are
provided in the README inside the data, code, and results folders
respectively.  Note that the code linked here is licensed under the <a href="http://www.gnu.org/copyleft/lesser.html">GNU Lesser
Public License</a>.

<strong> Teaching </strong>

This 'hockey problem' combines an intuitive estimation question (who
influences the outcome?) with difficult high-dimensional data (e.g., who was
on-ice?).  It turns out that the general example is a fantastic tool for
teaching.   Indeed, at the end of quarter I (MT) realized that I'd mentioned
the `hockey data' in almost every lecture of the <a
href="http://faculty.chicagobooth.edu/matt.taddy/teaching">data mining
class</a> that I teach at Chicago Booth.  Thus this repository will also
contain code to illustrate core data mining techniques, including principal
components regression, K-means clustering, random forests, and much else.
