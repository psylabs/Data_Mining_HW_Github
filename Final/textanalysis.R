# Data Mining Final
source("textstart.R")
library(textir)
library(maptpx)
# The first colname (word) is  "" 
# We would expect "" to show up in every speech
length(which(allwords %in% WORDS[1])) #only 1351 times
wordSM <- wordSM[,-1]  #getting rid of it

#Run marginal analysis to subset words to analyze
P <- as.data.frame(as.matrix(wordSM>0))
margreg <- function(p){
#   fit <- glm(OCdata$prefOC~p*OCdata$targetS - OCdata$targetS) #this should be done later
  fit <- glm(support~p, family="binomial")
  sf <- summary(fit)
  return(sf$coef[2,4]) 
  #Throw in FDR here
}

cl <- makeCluster(detectCores())
support <- OCdata$prefOC>6 #A tight cutoff gave us the most signal - 60 polarizing words
clusterExport(cl,"support") 
mrgpvals <- unlist(parLapply(cl,P,margreg))
stopCluster(cl) 

# Add back column names
names(mrgpvals) <- colnames(P)
hist(sort(mrgpvals)[1:250], col="gray", 
     border="yellow", xlab="p-values") 
names(sort(mrgpvals)[1:250])

# These are the words have the most predictive power (ignoring Beta)
# most are innocuous but some good ones: america, capitalist etc.
wordSMcut <- wordSM[,names(sort(mrgpvals)[1:60])] # ProbablyShouldn't use 250.

#Dimension Reduction method 2: Predict versus raw counts
ctfit <- glm(support~wordct*OCdata$targetS, family="binomial") #didnt work



## [2] linear regression for 1-7 OCpref on words (add in demographics later). 
linfit <- gamlr(wordSMcut, y=OCdata$prefOC,lambda.min.ratio=1e-3)
plot(linfit)
Blin <- drop(coef(linfit)) # AICc
Blin[order(Blin, decreasing=T)[1:40]]
#Now were getting somewhere Gamlr choses some 36 of 60 words
# For a one unit increase in the word capitalist 
# there is a 1.6 point increases in pref for OC (seems flipped)
yhat <- as.vector(predict(linfit,wordSMcut))
cor(OCdata$prefOC,yhat)^2 #IS R2 of 13%
#Interacting here with *OCdata$targetS doesnt improve the model

# [3a] Let's try the logistic OOS
y=OCdata$pref>6
cv.binfit=cv.gamlr(wordSMcut, y, family="binomial", lambda.min.ratio=1e-3)
plot(cv.binfit)
plot(cv.binfit$gamlr)
##OOS R2
1-cv.binfit$cvm[cv.binfit$seg.min]/cv.binfit$cvm[1] #Numbers vary wildly
1-cv.binfit$cvm[cv.binfit$seg.1se]/cv.binfit$cvm[1]

# [3b] Let's try the logistic interacted IS
binfit <- gamlr(wordSMcut*OCdata$targetS, y,family="binomial",lambda.min.ratio=1e-3)
plot(binfit)
Bbin <- drop(coef(binfit)) # AICc
Bbin[order(Bbin, decreasing=T)[1:40]] 
# a 1 unit increase in the word privelege increases the log odds of
yhat <- as.vector(predict(binfit,wordSMcut, type="response"))
cor(OCdata$prefOC,yhat)^2 #IS R2 of 5.8%; 10% without interaction

# [4] Topic Models

tpcs <- topics(wordSM,K=5*(1:5),tol=10)  
summary(tpcs, n=10) 

##SCRAP
head(wordtrip)
reg1= cv.gamlr(wordSM, OCdata$supportOC)
regwords=coef(reg1)
regwords[order(regwords, decreasing=TRUE)[1:10]]

cv.binfithat <- predict(cv.binfit$gamlr, wordSMcut, type="response", lambda.min.ratio=1e-3) #Using AICc by default
plot(cv.binfithat,)

##Questions for MT
# Why does Gamlr fail with 250 words but work with 60
# Can a single glm command be run in parallel