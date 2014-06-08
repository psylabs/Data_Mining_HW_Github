#LOAD ALL THE DATA
OCdata <- read.csv("OCdata2.csv")
textmatch<-OCdata[,c("IDX", "exp")]
textmatch[,3:5]<-""

library(SnowballC)
library(stringr)
library(tm)
library(textir)
library(gamlr)
library(maptpx)
source("Phrases.R")
source("predreport.R")
allonewords<-""
alltwowords<-""
allthreewords<-""

for (i in 1:nrow(textmatch)){
  #PUTS ALL LETTERS IN LOWER CASE
  ex<-tolower(gsub("[[:punct:]]", "", textmatch[i,2]))
  #REMOVES "STOP WORDS" ("THE", "IT", "AND", "OF", etc.)
  ex<-stripWhitespace(removeWords(ex, stopwords("english")))
  #mikephrase("this is a test of my phrase chunking system")$twowords
  #SELF-IDENTIFYING WORDS stopwords("english")[-(1:8)]
  
  #STEMS THE REMAINING WORDS
  ex<-paste(stemexcept((strsplit(ex, split=" ")[[1]])), collapse=" ")
  ex2<-paste(apply(mikephrase(ex)$twowords,   1, function(x) paste0(x, collapse="")), collapse=" ")
  ex3<-paste(apply(mikephrase(ex)$threewords, 1, function(x) paste0(x, collapse="")), collapse=" ")
  #SAVES THE FORMATTED WORDS TO THE THIRD COLUMN
  textmatch[i,3]<-ex
  textmatch[i,4]<-ex2
  textmatch[i,5]<-ex3
  
  #CREATE A LIST OF EVERY WORD IN EVERY DESCRIPTION
  allonewords<-c(allonewords,     unlist(strsplit(textmatch[i,3], split=" ")))
  alltwowords<-c(alltwowords,     unlist(strsplit(textmatch[i,4], split=" ")))
  allthreewords<-c(allthreewords, unlist(strsplit(textmatch[i,5], split=" ")))
  print(i)}


TARGET<-which(OCdata$targetS==1)
YY<-OCdata[TARGET,]$prefOC
dt <- DocumentTermMatrix(Corpus(VectorSource(textmatch[,3])))
dtm<-dt[TARGET,]
dtm <- removeSparseTerms(dtm, sparse= 0.999)
ncol(dtm)
wordSM <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                        dims=c(dtm$nrow, dtm$ncol))
P <- as.data.frame(as.matrix(wordSM))
colnames(P)<-colnames(dtm)

# YOU LOSE ROWS IN 2 AND 3 WORD PHRASES, THIS IS HOW YOU FIND WHAT YOU LOSE
rowTotals <- apply(P , 1, sum) 
remove <- which(rowTotals==0); length(remove)

# [1A] RUN MARGINAL REG TO FIND SUBSET TO ANALYZE
margreg <- function(p){
  fit <- glm(YY~p)
  sf <- summary(fit)
  return(sf$coef[2,4]) 
  #Throw in FDR here
}

cl <- makeCluster(detectCores())
clusterExport(cl,"YY") 
mrgpvals <- unlist(parLapply(cl,P,margreg))
stopCluster(cl) 

names(mrgpvals) <- colnames(P)
hist(sort(mrgpvals)[1:250], col="gray", 
      xlab="p-values") 
names(sort(mrgpvals)[1:250])
length(which((sort(mrgpvals)[1:250])<.05)) #only * are <.05
#THIS IS FINE WE SHOULD LOOK AT INTERACTIONS NGRAMS,SPEECH LENGTH ETC 

# These are the words have the most predictive power (ignoring Beta)
wordSMcut <- wordSM[,names(sort(mrgpvals)[1:95])] # ProbablyShouldn't use 250.

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
pca <- prcomp(F, scale=TRUE)
plot(pca) 
summary(pca) # the PCs are explaining very little. Can you minimize the number of PCs like log bayes?
## look at the big rotations (it does a pretty good job!)
pca$rotation[order(abs(pca$rotation[,1]),decreasing=TRUE),1][1:10] #These rotations make sense.
pca$rotation[order(abs(pca$rotation[,2]),decreasing=TRUE),2][1:20] #no longer garbage

## Plot the first two PCs..
plot(pca$x[,1:2], col=0, xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n")
text(x=pca$x[1:20,1], y=pca$x[1:20,2], labels=colnames(wordSM2), cex=1)
#This isnt helpful but why are the all starting with a?
zpca <- predict(pca)
zdf <- as.data.frame(zpca)
pcreg<-glm(OCdata$prefOC ~ ., data=zdf)
summary(pcreg)

lassoPCR <- cv.gamlr(x=zdf, y=OCdata$prefOC>6, family="binomial", lambda.min.ratio=1e-2)
plot(lassoPCR$gamlr) #garbage
##OOS R2
1-lassoPCR$cvm[lassoPCR$seg.min]/cv.binfit$cvm[1]
1-lassoPCR$cvm[lassoPCR$seg.1se]/cv.binfit$cvm[1] 

cl <- makeCluster(detectCores())
myfit <- mnlm(cl, counts=wordSM2cut, covars =y, bins=2) 
# fits <- mnlm(cl, we8thereRatings, 
#              we8thereCounts, bins=5,nlambda=10)
stopCluster(cl)
B <- coef(myfit)
mean(B[-1,]==0) # sparsity in loadings
## some big loadings on `overall'
B[2,order(B[2,])[1:10]]
B[2,order(-B[2,])[1:10]]




#[6a] Trees
zpca <- predict(pca)
catree <- tree(y ~ as.matrix(wordSMcut))
rf <- randomForest(y ~ as.matrix(wordSM))

plot(catree, col=8, lwd=2)
text(catree)
carf <- randomForest(y ~ tpcs$omega,  ntree=250, nodesize=25, importance=TRUE)
