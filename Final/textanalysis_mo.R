#This file attempts to accomplish exactly what textstart does (+more) with Corpus()
library(stringr)
library(tm)
library(tree)
library(gamlr)
library(randomForest)
source("deviance.r")
OCdata <- read.csv("OCdata2.csv")
df <- data.frame(V1 = OCdata$exp, stringsAsFactors = FALSE)
mycorpus <- Corpus(DataframeSource(df))
dtm <- DocumentTermMatrix(mycorpus, control = list(removePunctuation = TRUE, stopwords = TRUE,minDocFreq=3, stemming = TRUE, removeNumbers = TRUE))
dtm <- removeSparseTerms(dtm, sparse= 0.999) #This is totally ballparked. mindocFreq above fails
# inspect(dtm[1:5,100:115])
wordSM2 <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                       dims=c(dtm$nrow, dtm$ncol))
colnames(wordSM2)<-colnames(dtm)
#NUMBER OF UNIQUE WORDS
dim(wordSM2)[2] #This works just as expected. 3352 words versus
dim(wordSM) #This has 3470 words
findFreqTerms(dtm, 400) #Most common terms.

# [1A] RUN MARGINAL REG TO FIND SUBSET TO ANALYZE
P <- as.data.frame(as.matrix(wordSM2>0))
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
length(which((sort(mrgpvals)[1:250])<.05)) #only 91 are <.05
#THIS IS FINE WE SHOULD LOOK AT INTERACTIONS NGRAMS,SPEECH LENGTH ETC 

# These are the words have the most predictive power (ignoring Beta)
wordSM2cut <- wordSM2[,names(sort(mrgpvals)[1:95])] # ProbablyShouldn't use 250.

# [1B] Dimension Reduction method 2: Predict versus raw counts
#ctfit <- glm(support~wordct, family="binomial") #didnt work



## [2a] linear regression for 1-7 OCpref on words
linfit <- gamlr(wordSM2cut, y=OCdata$prefOC,lambda.min.ratio=1e-3)
plot(linfit)
Blin <- drop(coef(linfit)) # AICc
Blin[order(Blin, decreasing=T)[1:40]]
#Now were getting somewhere Gamlr choses some 60 of the 95 words
# For a one unit increase in the word capitalist 
# there is a 1.6 point increases in pref for OC (seems flipped)
yhat <- as.vector(predict(linfit,wordSM2cut))
cor(OCdata$prefOC,yhat)^2 #IS R2 of 9.3%

## [2b] cv.linear regression for 1-7 OCpref on words 
cv.linfit <- cv.gamlr(wordSM2cut, y=OCdata$prefOC,lambda.min.ratio=1e-3)
plot(cv.linfit)
Bcvlin <- drop(coef(cv.linfit)) # AICc
Bcvlin[order(Bcvlin, decreasing=T)[1:40]]
#Now were getting somewhere Gamlr choses some 60 of the 95 words
# For a one unit increase in the word capitalist 
# there is a 1.6 point increases in pref for OC (seems flipped)
yhat <- as.vector(predict(cv.linfit,wordSM2cut))
cor(OCdata$prefOC,yhat)^2 #CV OOS r2 of 4.8



#[3a] Let's try the logistic IS
y=OCdata$prefOC>6
binfit <- gamlr(wordSM2cut, y,family="binomial",lambda.min.ratio=1e-3)
plot(binfit)
Bbin <- drop(coef(binfit)) # AICc
Bbin[order(Bbin, decreasing=T)[1:40]] 
# a 1 unit increase in the word beyond increases the log odds of
yhat <- as.vector(predict(binfit,wordSM2cut, type="response"))
R2(y=y, pred=yhat, family="binomial") # 16% wohoo

# [3b] Let's try the logistic OOS
cv.binfit=cv.gamlr(wordSM2cut, y, family="binomial", lambda.min.ratio=1e-3)
plot(cv.binfit)
plot(cv.binfit$gamlr)
##OOS R2
1-cv.binfit$cvm[cv.binfit$seg.min]/cv.binfit$cvm[1] #Why does this suck so bad?
1-cv.binfit$cvm[cv.binfit$seg.1se]/cv.binfit$cvm[1] #0?



# [4a] Topic Models
F <- wordSM2/rowSums(wordSM2) ## WE SHOULD BE DOING MORE OF THIS all models control for windbags
tpcs <- topics(F,K=5*(1:5),tol=10)  
#ToDo: Find counts of word in data
#These topics are amazing, terms are super intuitively correlated.
summary(tpcs, n=10) 
rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]] #Not sure why this answer is not same as bove

par(srt=-30, xpd=NA) ## rotate stings, and allow words outside plot
plot(tpcs$omega[,1], type="l", col=8, xlab="", xlim=c(0.5,12),
     xaxt="n", ylab="topic 1 weight", bty="n")
text(x=1:nrow(tpcs$omega), y=tpcs$omega[,1], labels=colnames(wordSM2))

# [4b] Topic Model regression IS
treg<-gamlr(tpcs$omega, y=OCdata$prefOC,lambda.min.ratio=1e-3)
plot(treg)
Btreg <- drop(coef(treg)) # AICc
Btreg[order(Btreg, decreasing=T)[1:11]]
drop(coef(treg))*0.1
yhat <- as.vector(predict(treg,tpcs$omega))
cor(OCdata$prefOC,yhat)^2 #IS R2 of 2% crapppy! 

# [4c] Topic Model regression OOS
regtpcs.cv <- cv.gamlr(tpcs$omega, y=OCdata$prefOC)
plot(regtpcs.cv);mtext("topic regression", font=2, line=2)
max(1-regtpcs.cv$cvm/regtpcs.cv$cvm[1]) #OOS R2 of 0.3% Geezus

# [4d] Logistic Topic Reg IS
tpcs.binfit <- gamlr(tpcs$omega, y,family="binomial",lambda.min.ratio=1e-3)
plot(tpcs.binfit) #Crap I was optimistic about this, but it's no good




# [5a] PCA
classpca <- prcomp(F, scale=TRUE)
plot(classpca) 

## look at the big rotations (it does a pretty good job!)
classpca$rotation[order(abs(classpca$rotation[,1]),decreasing=TRUE),1][1:10]
classpca$rotation[order(abs(classpca$rotation[,2]),decreasing=TRUE),2][1:20] #no longer garbage

## Plot the first two PCs..
plot(classpca$x[,1:2], col=0, xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n")
text(x=classpca$x[,1], y=classpca$x[,2], labels=colnames(wordSM2)[1:20], cex=0.5)
#This isnt helpful but why are the all starting with a?
zpca <- predict(classpca)
catree <- tree(y ~ as.matrix(wordSMcut))
rf <- randomForest(y ~ as.matrix(wordSM))

plot(catree, col=8, lwd=2)
text(catree)
carf <- randomForest(y ~ tpcs$omega,  ntree=250, nodesize=25, importance=TRUE)