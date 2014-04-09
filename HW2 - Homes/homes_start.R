##### ******** Mortgage and Home Sales Data ******** #####

## Read in the data
homes <- read.csv("homes2004.csv")
plot(homes$VALUE ~ homes$NUNITS)

# conditional vs marginal value
par(mfrow=c(1,2)) # 1 row, 2 columns of plots 
hist(homes$VALUE, col="grey", xlab="home value", main="")
plot(VALUE ~ factor(BATHS), 
    col=rainbow(8), data=homes[homes$BATHS<8,],
    xlab="number of bathrooms", ylab="home value")

# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
	factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

# some quick plots.  Do more to build your intuition!
par(mfrow=c(1,2)) 
plot(VALUE ~ STATE, data=homes, 
	col=rainbow(nlevels(homes$STATE)), 
	ylim=c(0,10^6), cex.axis=.65)
plot(gt20dwn ~ FRSTHO, data=homes, 
	col=c(1,3), xlab="Buyer's First Home?", 
	ylab="Greater than 20% down")

## code hints 

## Q2 
# regress log(VALUE) on everything except AMMORT and LPRICE 
pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
# extract pvalues
pvals <- summary(pricey)$coef[-1,4]
# example: those variable insignificant at alpha=0.2
names(pvals)[pvals>.2]
# you'll want to replace .2 with your FDR cutoff
# you can use the `-AMMORT' type syntax to drop variables

## Q3: 
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
# ybar and null deviance
source("deviance.R")
ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn, pred=ybar, family="binomial")




