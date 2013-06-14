Data Mining via Hockey
======

This repo will eventually contain a series of teaching examples motivated by the measurement of player ability in the NHL.  It is descendant from the <a href="http://faculty.chicagobooth.edu/matt.taddy/teaching">data mining class</a> that I teach at Chicago Booth.  The goal is to organize a set of DM examples based on a single data problem, and to experiment in alternative mediums for communicating and educating about data mining.

<strong> Background </strong>

Two colleagues and I wrote an academic paper that uses relatively new techniques to estimate hockey player performance.  The <a href="http://arxiv.org/abs/1209.5026">paper</a>, written with 
<a href="http://faculty.chicagobooth.edu/robert.gramacy">Bobby Gramacy</a> and <a href="http://www-stat.wharton.upenn.edu/~stjensen">Shane Jensen</a>, applies regularized logistic regression to predict the probability that either team on the ice scored any given goal.  The contribution of individual players to this for-vs-against goal probability is then taken as a measurement of their on-ice contribution.  It is a sort of regression-adjusted Plus-Minus.

This 'hockey problem' combines an intuitive estimation question (who influences the outcome?) with difficult high-dimensional data (e.g., who was on-ice?).  It turns out that the general example is a fantastic tool for teaching data mining methods.  Indeed, at the end of quarter I realized that I'd mentioned the `hockey data' in almost every lecture.  Thus the idea of this repository was born...

<strong> Content </strong>

The plan is to include R code to first pull the relevant data from nhl.com, and then do a series of analyses using techniques that include sparse regularized regression, principal components regression, K-means clustering, random forests, and much else.  The documentation will consist of lecture slides outlining the examples and referencing where the corresponding technique is described in my class notes.  This will be built at first by myself and an RA, but after finishing the core material my hope is that others will begin to contribute their own approaches to the problem.
