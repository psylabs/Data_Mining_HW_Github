 #LOAD ALL THE DATA
OCdata <- read.csv("OCdata.csv")

#BREAK OFF THE TEXT WITH ID CODES
textmatch<-OCdata[,c("IDX", "exp")]
textmatch[,3]<-""

library(tm)
library(SnowballC)
for (i in 1:nrow(textmatch)){
  #PUTS ALL LETTERS IN LOWER CASE
  ex<-tolower(gsub("[[:punct:]]", "", textmatch[i,2]))
  #REMOVES "STOP WORDS" ("THE", "IT", "AND", "OF", etc.)
  ex<-stripWhitespace(removeWords(ex, stopwords("english")))
  #STEMS THE REMAINING WORDS
  ex<-paste(wordStem((strsplit(ex, split=" ")[[1]]), language="english"), collapse=" ")
  #SAVES THE FORMATTED WORDS TO THE THIRD COLUMN
  textmatch[i,3]<-ex
print(i)}

#CREATE A LIST OF EVERY WORD IN EVERY DESCRIPTION
allwords<-""
for (i in 1:nrow(textmatch)){
  #ADDS WORDS FROM THIS ROW INTO THE LIST
  allwords<-c(allwords, unlist(strsplit(textmatch[i,3], split=" ")))
print(i)}

#NUMBER OF WORDS
length(allwords)
#NUMBER OF UNIQUE WORDS
WORDS<-unique(allwords)
length(WORDS)

#COUNT OF HOW MANY TIMES EACH WORD IS WITHIN EACH DESCRIPTION
wordct<-matrix(data=NA, nrow=nrow(textmatch), ncol=length(WORDS))
wordtrip<-matrix(data=NA, nrow=nrow(textmatch)*length(WORDS), ncol=3)
for (i in 1:nrow(textmatch)){
  for (j in 1:length(WORDS)){
    wordct[i,j]<-sum(unlist(strsplit(textmatch[i,3], split=" "))
                     ==WORDS[j])
    TN<-(i-1)+(j-1)*nrow(textmatch)
    wordtrip[TN,1]<-i
    wordtrip[TN,2]<-j
    wordtrip[TN,3]<-wordct[i,j]
  }
print(i)}


WT<-wordtrip[-which(wordtrip[,3]==0),]
WT<-WT[-which(is.na(WT[,1])),]
wordSM<-sparseMatrix(i=WT[,1],j=WT[,2],x=WT[,3])

colnames(wordSM)=WORDS

