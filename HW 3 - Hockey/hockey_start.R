## nhl hockey analysis

## the data is in gamlr.  
## You need to first install this, 
## via install.packages("gamlr")

library(gamlr) # loads Matrix as well
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

##### Processing Detail: 
## build matrices of team, situation, and player effects
#
# Create home and away team indicator matrices 
# first, make sure the factorization levels match up
all(levels(goal$team.home)==levels(goal$team.away))
teams <- levels(goal$team.home) #our list of teams

# An aside... something I get asked often: 
#   how do I get indicators for all levels of a factor?
#   i.e., can we avoid dropping the reference level?
# Easiest thing to do is create an extra factor level as reference
goal$team.home <- factor(goal$team.home, levels=c(NA,teams), exclude=NULL)
goal$team.away <- factor(goal$team.away, levels=c(NA,teams), exclude=NULL)
# do a similar thing for goal$season
goal$season <- as.factor(goal$season) # first convert from numeric, then:
goal$season <- factor(goal$season, levels=c(NA,levels(goal$season)), exclude=NULL)
## The exclude=NULL argument is necessary so that R doesn't skip the NA.
## Now the factors have reference level NA: R's symbol for missing data.
levels(goal$team.home)
levels(goal$season)

## get a separate effect for each team in each season
# home team indicators
homemat <- sparse.model.matrix(~ team.home*season, data=goal)[,-1]
# away team version
awaymat <- sparse.model.matrix(~ team.away*season, data=goal)[,-1]
# column names
colnames(homemat)
colnames(awaymat)
# combine them: +1 for home, -1 for away
xteam <- suppressWarnings(homemat-awaymat) # warns about colnames not matching (is OK)
# because I'm obsessive about sensical names
colnames(xteam) <- sub("team.home","",colnames(xteam)) # drop `team.home' from varnames
xteam[1,] # goal 1 is in a game of DAL @ EDM

# also, config contains play configuration info
# e.g., S5v4 is 5 on 4 hockey, +1 if it is for home-team and -1 for away team
config[1,]


##### Analysis

# Combine the covariates all together
x <- cBind(config,xteam,onice) # cBind binds together two sparse matrices
# build 'y': home vs away, binary response
y <- goal$whoscored=="HOME"

nhlreg <- gamlr(x, y, 
	free=1:(ncol(config)+ncol(xteam)), ## free denotes unpenalized columns
	family="binomial", standardize=FALSE)

## coefficients (grab only the players)
# AICc selection 
Baicc <- coef(nhlreg)[colnames(onice),]


## Mo Code
B <- coef(nhlreg) ## the coefficients selected under AICc
B <- B[-1,] # drop intercept and remove STM formatting
length(B <- B[B!=0]) #989 are sig

B[which.min(Baicc)] ## Worst Player
B[which.max(Baicc)] ## Best Player

cv.nhlreg <- cv.gamlr(x, y, verb=TRUE)
## plot them together
par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(cv.nhlreg$gamlr) ## cv.gamlr includes a gamlr object
