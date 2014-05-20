# HW 3 

source("microfi_start.R")
ld=log(degree+1)
hh$loan=factor(hh$loan)
hh$electricity=factor(hh$electricity)
hh$leader=factor(hh$leader)

par(mfrow=c(1,2))
hist(degree, xlim=c(0,50))
hist(ld, xlab="log(degree+1)")
par(mfrow=c(1,1), ps=25)


#Q.2
controls <- data.frame(hh[,c(2:9)])
x = model.matrix(~.^2, data=controls)[,-1]
reg1_log = gamlr(x,ld) #what happens to lambdas?
#B=coef(reg1_log)[-1,]; B[order(B, decreasing=TRUE)[1:20]] #If you want to see what predicts connectedness
ldhat <- predict(reg1_log, x, type="response")
cor(drop(ldhat),ld)^2 #R2=10.7%
plot(ld,ldhat,xpd=F,main="Model fit for log(degree+1) "); abline(a=0,b=1, col="red")

#Q.3
fit1 <- gamlr(cBind(ld,ldhat,x),hh$loan,free=2,  family="binomial")
exp(coef(fit1)["ld",]) #odds of taking a loan increase by 14% for an increase in connectedness of 1 degree


#Q.4 Compare the results from [3] to those from a straight (naive) lasso
# for loan on d and x. Explain why they are similar or different.

## naive lasso regression
naive <- gamlr(cBind(ld,x),hh$loan, family="binomial") #Why doesnt he set free=1 here??
exp(coef(naive)["ld",]) #14.2%

#5. Bootstrap
n <- nrow(hh)
gamb <- c() # empty gamma
for(b in 1:20){
  ## create a matrix of resampled indices
  ib <- sample(1:n, n, replace=TRUE)
  ## create the resampled data
  xb <- x[ib,]
  ldegreeb <- ld[ib]
  loanb <- hh$loan[ib]
  ## run the treatment regression
  treatb <- gamlr(xb,ldegreeb,lambda.min.ratio=1e-3)
  ldegreehatb <- predict(treatb, xb, type="response")
  
  fitb <- gamlr(cBind(ldegreeb,ldegreehatb,xb),loanb,free=2,  family="binomial")
  gamb <- c(gamb,coef(fitb)["ldegreeb",])
  print(b)
}
## not very exciting though: all zeros
hist(gamb, main="Expected effect of degree in log odds") ; abline(v=mean(gamb), col="red")
1-fitb$deviance/fitb$deviance[1] #different way of calculating r2



##### Solution
lapply(hh,summary) # look at summary of each row
# use naref to get relevel factors, you dont want them to be subsumed into the intercept
# Why do we do this - cuz in the lasso the ref matters, and the intercept choice penalizes the factors.
# Earlier taught that factors should fit into the intercept, but this is no longer true, cuz we penalize Betas. Lambdas shrink
# the intercept to  0 or if we fail to naref then to village 1 (which is not the intercept), if we want the average village use naref 
source("naref.r")
hhnoref = naref(hh)

