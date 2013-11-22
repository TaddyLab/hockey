#Comparison of two salary data sets

**salary_bh** (the salary data obtained from Blackhawk Zone): 
	
	number of players: 2442, salary info: season_2002-season_2013
	
**salary_hz** (the salary data obtained from Hockeyzone Plus):
	
	number of players: 1853 (1850 with valid names), salary info: season_2002-season_2011
	
###Issues found in the 'salary_hz'
3 players in the salary_hz data set do not have complete names. I can not figure out who they are.

	  		NAME X0203US X0304US X0506US X0607US X0708US X0809US X0910US X1011US X1112US TOTAL
	  1247  York     1.2       0       0       0       0       0       0       0       0   1.2
	         NAME X0203US X0304US X0506US X0607US X0708US X0809US X0910US X1011US X1112US TOTAL
	  1456  Colin    0.75       0       0       0       0       0       0       0       0  0.75
	          NAME X0203US X0304US X0506US X0607US X0708US X0809US X0910US X1011US X1112US TOTAL
	  1834  Dewolf     0.4       0       0       0       0       0       0       0       0   0.4

1 duplicate salary records:

            	Name Salary.0203 Salary.0304 Salary.0506 Salary.0607 Salary.0708 Salary.0809
	957  JOE_DIPENTA           0        0.55        0.45         0.5         0.7           0
	1474 JOE_DIPENTA           0        0.00        0.00         0.0         0.7           0
     	 Salary.0910 Salary.1011 Salary.1112 included_hz
	957            0           0           0        TRUE
	1474           0           0           0        TRUE
	
###Simple comparisons of two data sets
Common records (matching records): 838

Conflicting records (different salary info for the same player): 799

In salary_bh but not in salary_hz: 805

In salary_hz but not in salary_bh: 212

** Note: records in one data set but not in another one may because of the different names for the same player, e.g. 'STEVE_REINPRECHT' in salary_hz while 'STEVEN_REINPRECHT' in salary_bh. **
###Further comparisons
