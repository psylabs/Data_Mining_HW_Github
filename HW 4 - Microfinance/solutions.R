## microfinance network 
source("microfi_start.R")

# create a sparse model matrix x and response y
# first, remove reference levels for each factor
source("naref.R")
hhnoref <- naref(hh)
## I looked all controls interacted.
## No big deal if you just did main effects.
x <- sparse.model.matrix(loan ~.^2, data=hhnoref)[,-1]
y <- hh$loan

## [1]
## look at degree
hist(degree)
## it looks like it is more normal on log scale...
## this is important because we need to predict it for control (dhat)
## for example, look what happens if you try a linear model for degree:
treat <- gamlr(x,degree) 
degreehat <- predict(treat, x)
plot(degree,degreehat)
## looks like nonconstant variance...
## Instead, we'll look at log(1+degree) as our treatment
d <- log(1+degree)
## this also makes sense if you think about the concept of `connectivity':
## moving from 0 to 1 friends is a bigger change than from 50 to 51.

## [2]
## predicting treatment (degree)
cv.treat <- cv.gamlr(x,d) 
# OOS R^2 is around 7%, so lots of independent movement to measure
1-cv.treat$cvm[cv.treat$seg.min]/cv.treat$cvm[1] 
# I've used AICc for selecting dhat; its fine if you used something else.
dhat <- predict(cv.treat$gamlr, x)
# Lots of in-sample variation.
# still some nonconstant variance, but looks better to me than for raw degree.
plot(d,dhat)
# note that in-sample R2 is really what determines how much independent
# variation is available for estimating the causal treatment effect.
cor(d,drop(dhat))^2 ## here, IS R2 is only around 11%.  


## [3]
causal <- gamlr(cBind(d,dhat,x),y,family="binomial",free=2)
plot(causal)
coef(causal)["d",] # around 0.13
## An effect!  so at least amongst these controls, we've got evidence
## that connectivity and propensity to borrow money are related.  
## open question: which way does the causation run?
## Interpretation:
## This is a log-log regression!  log odds on log degree (+1).
## So like with the OJ, example, we can interpret 0.13 
## as % increase in odds of taking a loan per 1% increase in degree.

## [4]
## naive lasso
naive <- gamlr(cBind(d,x),y,family="binomial")
coef(naive)["d",]
## EXTRA: marginal regression too
marginal <- glm(y~d,family="binomial")
coef(marginal)["d"]
## these are pretty close here, because treatment is already
## mostly independent from response 
## (d is not easy to predict, as we saw in [2])
## Note also that the effect _increases_ in magnitude after adding controls.
#   
# Controlling for confounders doesn't always reduce causal effects.  
# This only happens if confounders have the same sign on their correlation with both
# treatment and response.  If these signs are different, then control
# can give you a bigger effect than you would otherwise get.
#
# Think about a study where only the sickest people get some treatment.
# Then if you don't control for how sick they are, and compare them to
# [healthier] people who didn't get the treatment, you'll conclude the
# drug is less effective than it actually is.

## [5]
## bootstrap it; in parallel!
## You could also have done this in serial no prob.
## Note that the BCH algo takes too long and is unstable here;
## that's why I didn't get you to bootstrap it
library(parallel) 
cl = makeCluster(detectCores())
n <- nrow(hh)
B <- 100 
resamp <- as.data.frame(
	matrix(sample(1:n, B*n, replace=TRUE),ncol=B))

## a fit function (basically same as in abortion.R, but binomial)
bootfit <- function(ib){ 
	require(gamlr)
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	treatb <- gamlr(xb,db)
	dhatb <- predict(treatb,xb)
	fitb <- gamlr(cBind(db,dhatb,xb),
				yb,free=2,family="binomial")
	return(coef(fitb)["db",])
}

clusterExport(cl, c("x","d","y"))
bootgamma <- unlist(parLapply(cl,resamp,bootfit))
stopCluster(cl) 

## finally, plots.  These summarize the sampling distribution
## for 'gamma hat' estimated under our lasso treatment effect routine.
## It includes uncertainty from resampling both X and Y.
hist(bootgamma, col="grey50", 
	border="yellow", xlab="log network degree effect", main="")
abline(v=coef(causal)["d",],lwd=2)

## BONUS POINTS TO ANYONE WHO RECALCULATED DEGREE FOR EACH BOOT!!!

## [+]
## alternatively, the BCH approach
dpreds <- which(coef(cv.treat$gamlr)[-1]!=0) # from dhat model above
yonx <- gamlr(x,y) # predict y from x without d
ypreds <- which(coef(yonx)[-1]!=0) # grab the preds
## get the union of controls that predict y or d
inthemodel <- unique(c(dpreds,ypreds)) 
selectdata <- cBind(d,x[,inthemodel]) 
selectdata <- as.data.frame(as.matrix(selectdata)) 
dim(selectdata) 
causal_glm <- glm(y~., data=selectdata, family=binomial) ## run a glm
summary(causal_glm)$coef["d",] # still significant, even bigger estimate

## add results to the plot from [5]
hist(bootgamma, col="grey50", 
	border="grey90", xlab="log network degree effect", main="")
abline(v=coef(causal)["d",],lwd=2)
bch <- coef(summary(causal_glm))["d",]
bchgam <- bch[1]
bchse <- bch[2]
polygon(x=c(rep(bchgam-2*bchse,2),rep(bchgam+2*bchse,2)),
		y=c(0,100,100,0), col=rgb(0,1,0,.25), border=NA)
abline(v=bchgam,col="green")
## the BCH result actually looks closer to our bootstrap mean

## EXTRA content... 
## Another version of `causal' that I like even better:
## Let all village `fixed effects' enter unpenalized
## (we have enough data, and this is closer to a blocked experiment)
colnames(x)[1:38]  # village effect is first 38 columns of my x
## say vc stands for 'village controls'
causal_vc <- gamlr(cBind(d,dhat,x),y,family="binomial",
	free=c(2,2+1:38)) # why 2+1:38?
coef(causal_vc)["d",] # result doesn't change much.

#########  WEEK 9 RANDOM FORESTS STUFF

## use RF to estimate dhat
library(randomForest) 
## RFs do a bad job with high dimensional factors (e.g. village)
## so we'll do as I do above in `EXTRA' and just include that unpenalized.
## now we need to get a model for dhat from all other controls
hh$loan <- factor(hh$loan) # RF likes you to do this first
rf.treat <- randomForest(d~.-village-loan, data=hh, ntree=250, nodesize=25)

rf.dhat <- predict(rf.treat, newdata=hh)
plot(d,rf.dhat)
cor(d,rf.dhat)^2 # lower than above, but we didn't include village

rf.causal <- gamlr(cBind(d,rf.dhat,x),y,family="binomial",
					free=c(2,2+1:38)) # village enters unpenalized

coef(rf.causal)["d",] # result drops to around 0.1; not a big diff.


