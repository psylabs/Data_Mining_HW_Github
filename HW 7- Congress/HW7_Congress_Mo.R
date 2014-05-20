# Data Mining [Matt Taddy]
# HW 7 - Congress
library(textir)
source("kIC.R") 
library(maptpx)
library(gamlr)
data(congress109)

x<- scale(congress109Counts) #scale the data
# Averaging by rowsums doesnt work for some reason?

kcfit <- lapply(1:25, function(k) kmeans(x,k,nstart=10))
kcbic <- sapply(kcfit,kIC,"B")
## plot 'em
plot(kcbic, xlab="K", ylab="IC", 
     xlim=c(0,25),ylim=range(kcbic),
     bty="n", type="l", lwd=2)
abline(v=which.min(kcbic))
#BIC suggest a model of K=15

#Interpret this model
k=15
1 - sum(kcfit[[k]]$tot.withinss)/kcfit[[k]]$totss
# Crap! IS Fit of <10% is poor.
a=(tapply(congress109Ideology$party,kcfit[[k]]$cluster,table))
as.data.frame(a)
#Looks like there are 1 person clusters, this is no good.
# No clear D Vs R split clusters
print(apply(kcfit[[k]]$centers,1,function(c) colnames(x)[order(-c)[1:10]])) #Cluster 7 is republican


# [Q.2]
stm <-as.simple_triplet_matrix(congress109Counts) #No clue whyy I'm doing this
tpcs <- topics(stm,K=5*(1:5),tol=10)  
summary(tpcs, n=10) 


#[Q.3]
x2<-100*congress109Counts/rowSums(congress109Counts)
#This data is unscaled meaning the means are not 0 and sdev!=1 but we are penalizing people for being talkative.
kcfit2 <- lapply(1:25, function(k) kmeans(x,k,nstart=10))
kcbic2 <- sapply(kcfit2,kIC,"B")
## plot 'em
plot(kcbic2, xlab="K", ylab="IC", 
     xlim=c(0,25),ylim=range(kcbic),
     bty="n", type="l", lwd=2)
abline(v=which.min(kcbic2)) #14
#The results are similar.
k=which.min(kcbic2)
ideology=tapply(congress109Ideology$party,kcfit2[[k]]$cluster,table)
as.data.frame(ideology) #slightly better output
#let's look at k_12
kcfit2[[k]]$centers[12]
print(apply(kcfit2[[k]]$centers,1,function(c) colnames(x)[order(-c)[1:10]]))

#[Q.3][b]
tpcreg <- gamlr(tpcs$omega,congress109Ideology$repshare) #what is repshare? [how republican(bush friendly) their constituency is?]
plot(tpcreg)
drop(coef(tpcreg))*0.1


library(glmnet)
par(mfrow=c(1,1))
tpcreg2 <- cv.glmnet(tpcs$omega, congress109Ideology$party, family="multinomial")
plot(tpcreg2)
b <- coef(tpcreg2)
probreg2 <- predict(tpcreg2, tpcs$omega, type="response")
trueprobs <- probreg2[cbind(1:length(tpcs$omega), congress109Ideology$party)] 
plot(trueprobs ~ congress109Ideology$party, col="lavender", varwidth=TRUE,
     xlab="party", ylab="prob( true class )") 

probfgl <- drop(probfgl)


