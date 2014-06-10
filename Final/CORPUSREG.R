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
dtm <- removeSparseTerms(dtm, sparse= 0.991)
ncol(dtm)
wordSM2 <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                        dims=c(dtm$nrow, dtm$ncol))
P <- as.data.frame(as.matrix(wordSM2))
colnames(P)<-colnames(dtm)

rowTotals <- apply(P , 1, sum) #Find the sum of words in each Document
remove <- which(rowTotals==0); length(remove)

#GAMLR on all words
linfit <- gamlr(P, y=YY, lambda.min.ratio=1e-2)
plot(linfit)
Blin <- drop(coef(linfit)) # AICc
#Blin[order(Blin, decreasing=T)[1:20]]
Blin[which(abs(Blin)>0)]
yhat <- as.vector(predict(linfit,P))
cor(YY,yhat)^2 
hist(yhat)

#PCA
pca <- prcomp(na.omit(P), scaled=TRUE)
plot(pca) #First PCAs much better than others, this is good
summary(pca) 
pca$rotation[order(abs(pca$rotation[,1]),decreasing=TRUE),3][1:10] #These rotations make sense.
pca$rotation[order(abs(pca$rotation[,5]),decreasing=TRUE),5][1:10] #no longer garbage
pca$rotation[order(abs(pca$rotation[,7]),decreasing=TRUE),7][1:10]

zpca <- predict(pca)
zdf <- as.data.frame(zpca)
pcreg<-glm(YY ~ ., data=zdf)
summary(pcreg)
yhat <- predict(pcreg,as.data.frame(zdf))
cor(YY,yhat)^2 


#TOPICS LOOPED
TMcol<-3
yhat<-list()
dev.off()
par(mfrow=c(1,2), cex=1.5)
for (TT in 0:1){
  TARGET<-which(OCdata$targetS==TT)
  YY<-OCdata[TARGET,]$prefOC
  dt <- DocumentTermMatrix(Corpus(VectorSource(textmatch[,TMcol])))
  dtm <- removeSparseTerms(dt[TARGET,], sparse= 0.991);ncol(dtm)
  # [4a] BUILD TOPIC MODEL  
  P <- as.matrix(dtm)/rowSums(as.matrix(dtm)); dim(P)
#   rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#   remove <- which(rowTotals==0); length(remove)
  tpcs <- topics(P,K=5*(1:5),tol=10)  
  summary(tpcs, n=10)   
  # [4b] IS TOPIC MODEL GAMLR
  treg<-gamlr(tpcs$omega, y=YY,lambda.min.ratio=1e-3)
  plot(treg, main=paste("Topic Reg writing TargetS =", TT, sep=" "))
  print(round(drop(coef(treg)),1)) #AICc #Shows topics support or oppose OC  
#   cor(YY,yhat)^2
#   yhat<-predict(treg,tpcs$omega)
  yhat[[TT+1]] <- as.vector(predict(treg,tpcs$omega))
}

predreport(yhat[[2]], yhat[[1]])



#MNIR looped
TMcol<-5
yhat<-list()
dev.off()
for (TT in 0:1){
  TARGET<-which(OCdata$targetS==TT)
  YY<-OCdata[TARGET,]$prefOC
  dt <- DocumentTermMatrix(Corpus(VectorSource(textmatch[,TMcol])))
  dtm <- removeSparseTerms(dt[TARGET,], sparse= 0.999)
  wordSM2 <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                          dims=c(dtm$nrow, dtm$ncol))
  P <- as.data.frame(as.matrix(wordSM2)); length(P)
  colnames(P)<-colnames(dtm)
  cl=NULL
  fitCS <- mnlm(cl, YY, P, bins=5,gamma=1)
  B <- coef(fitCS)
  print(B[2,order(B[2,])[1:10]])
  print(B[2,order(-B[2,])[1:10]])
  z <- srproj(B,as.matrix(P))
  print(summary(fwd <- lm(YY ~ z)))
  par(mfrow=c(1,2),cex=1.5)
  #PLOT THE LINEAR MODEL        
  plot(fwd$fitted ~ factor(YY), 
       varwidth=TRUE, col="lightslategrey",
       xlab="PrefOC", ylab="z", pch=20,
       main=paste("LM fit from MNIR targetS =", TT, sep=" ")) 
  #PLOT Z VS M(PHRASE LENGTH)
  plot(z, pch=20, main=paste("SR Projection targetS=", TT, sep=" "),
       ylab="number of phrases", xlab="z")  
  mnirfit <- gamlr(z, y=YY, lambda.min.ratio=1e-2)
  Bmnir <- drop(coef(mnirfit)) # AICc
  Bmnir[which(abs(Bmnir)>0)]  
  yhat[[TT+1]] <- as.vector(predict(mnirfit,z))
}

predreport(yhat[[2]], yhat[[1]])