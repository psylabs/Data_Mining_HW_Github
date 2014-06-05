source("Phrases.R")

#LOAD ALL THE DATA
OCdata <- read.csv("C:/Users/Mike/Google Drive/Data Mining/finalproject/OCdata2.csv")

#BREAK OFF THE TEXT WITH ID CODES
textmatch<-OCdata[,c("IDX", "exp")]
textmatch[,3:5]<-""


library(SnowballC)
library(stringr)
library(tm)
library(textir)
library(DAAG)
library(gamlr)
library(maptpx)

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
dt <- DocumentTermMatrix(Corpus(VectorSource(textmatch[,4])))
dtm<-dt[TARGET,]
dtm <- removeSparseTerms(dtm, sparse= 0.994)
ncol(dtm)

wordSM2 <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,
                        dims=c(dtm$nrow, dtm$ncol))
P <- as.data.frame(as.matrix(wordSM2))
colnames(P)<-colnames(dtm)

linfit <- gamlr(P, y=YY, 
                lambda.min.ratio=1e-2)
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

pca <- predict(pca)
zdf <- as.data.frame(zpca)
pcreg<-glm(YY ~ ., data=zdf)
summary(pcreg)
yhat <- predict(pcreg,as.data.frame(zdf))
cor(YY,yhat)^2 


draws<-vector()
for(i in 1:10){
  Spred<-(1*(yhat>median(yhat))
          +1*(yhat==median(yhat)*sample(c(0,1), length(yhat), replace=T)))
  
  draws[i]<-mean(Spred==OCdata[TARGET,]$supportOC)
}
print(100*round(mean(draws), 3))


  



# lassoPCR <- cv.gamlr(x=zdf, y=YY, lambda.min.ratio=1e-2)
# coef(lassoPCR)
# plot(lassoPCR$gamlr) #garbage
# ##OOS R2
# 1-lassoPCR$cvm[lassoPCR$seg.min]/cv.binfit$cvm[1]
# 1-lassoPCR$cvm[lassoPCR$seg.1se]/cv.binfit$cvm[1] 