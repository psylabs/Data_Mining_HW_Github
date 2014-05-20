
wine <- read.csv("wine.csv")

## scale
xwine <- scale(wine[,1:11])
apply(xwine,2,sd) # sd=1
apply(xwine,2,mean) # mean=0

## fit two clusters
two <- kmeans(xwine,2,nstart=10)
two$centers # big differences on all accounts
# what is the color distribution in each?
tapply(wine$color,two$cluster,table)
# the two clusters are red v. white!

# randomize order in plot, just so its not all white on top of red
# note that my cluster 1 was red; this could be flipped for you.
i <- sample(1:nrow(xwine))  
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
	pch=21, cex=.75, bty="n",
	xlab="fixed acidity",
	ylab="volatile acidity",
	bg=c("maroon","gold")[two$cluster[i]],
	col=c("maroon","gold")[wine$color[i]])

# create a big long vector of clusters
# takes a bit of time 
# you'll also get warnings of non-convergence;
# this isn't too bad; it just means things look 
# longer than the allotted 10 minimization iterations.
kfit <- lapply(1:200, function(k) kmeans(xwine,k))

# choose number of clusters?
source("kIC.R") ## utility script
# you give it kmeans fit, 
# then "A" for AICc (default) or "B" for BIC
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")
## plot 'em
plot(kaicc, xlab="K", ylab="IC", 
	ylim=range(c(kaicc,kbic)), # get them on same page
	bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
# both AICc and BIC choose very complicated models

# you get too big, and you lose interpretive power.
# no clear role of what to do here, it depends what you want.
# we'll just focus on k=30, where the BIC starts to level out

k=30
# IS R^2 of 70% (for explaining deviance of x)
1 - sum(kfit[[k]]$tot.withinss)/kfit[[k]]$totss
# clearly still splitting on color
tapply(wine$color,kfit[[k]]$cluster,table)
## look also at quality average by cluster
tapply(wine$quality,kfit[[k]]$cluster,mean)
# mostly all down around 5-6.5; no clear `star' clusters
# main sources of variation not driven by quality

## this isn't the same as x being unrelated to quality
library(glmnet)  
# create matrix of al three way interactions
x <- model.matrix(quality~.^3, data=wine)[,-1]
## note that since x is dense (i.e., not all zeros)
## glmnet is faster than gamlr for this (which is optimized for sparse x)
winereg <- cv.glmnet(x,wine$quality,lambda.min.ratio=1e-5)
plot(winereg)
max(1-winereg$cvm/winereg$cvm[1]) # max OOS R2 on y of about 1/3
