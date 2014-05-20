# Data Mining Final
source("textstart.R")
# The first colname (word) is  "" 
# We would expect "" to show up in every speech
length(which(allwords %in% WORDS[1])) #only 1351 times

#Run marginal analysis to subset words to analyze
P <- as.data.frame(as.matrix(wordSM>0))
margreg <- function(p){
  fit <- glm(OCdata$prefOC~p)
  sf <- summary(fit)
  return(sf$coef[2,4]) 
}

mrgpvals <- c()
for(j in 1:ncol(P)){
  print(j)
	mrgpvals <- c(mrgpvals,margreg(P[,j]))
}
# Add back column names
names(mrgpvals) <- colnames(P) # "" becomes V1
names(sort(mrgpvals)[1:250]) 
# These are the words have the most predictive power (ignoring Beta)
# most are innocuous but some good ones: socialist, capitalist etc.
wordSMcut <- wordSM[,names(sort(mrgpvals)[1:250])]

## [2] linear regression for 1-7 OCsupport on words (add in demographics later). 
linfit <- gamlr(wordSMcut, y=OCdata$supportOC,lambda.min.ratio=1e-3)
plot(linfit)
#It prefers the null model, this makes sense, look at the p-values they're weak.
hist(mrgpvals, col="gray", 
     border="yellow", xlab="p-values") #No signal

# [3] Let's try the logistic
y=OCdata$pref>4
cv.binfit=cv.gamlr(wordSMcut, y, family="binomial", lambda.min.ratio=1e-3)
plot(cv.binfit)
plot(cv.binfit$gamlr)
##OOS R2
1-cv.binfit$cvm[cv.binfit$seg.min]/cv.binfit$cvm[1]
1-cv.binfit$cvm[cv.binfit$seg.1se]/cv.binfit$cvm[1]



##SCRAP
head(wordtrip)
reg1= cv.gamlr(wordSM, OCdata$supportOC)
regwords=coef(reg1)
regwords[order(regwords, decreasing=TRUE)[1:10]]

cv.binfithat <- predict(cv.binfit$gamlr, wordSMcut, type="response", lambda.min.ratio=1e-3) #Using AICc by default
plot(cv.binfithat,)
