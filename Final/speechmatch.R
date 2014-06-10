#setwd("C:/Users/Mike/Google Drive/Data Mining/finalproject/")

SPEECH <- read.csv("speeches.csv")
SPEECH<-SPEECH[which(nchar(as.character(SPEECH$Passign))==1),]
SPEECH$Passign[which((SPEECH$Passign)=="I")]<-"D"
SPEECH$Passign<-factor(SPEECH$Passign)
summary(SPEECH$Passign)

source("Phrases.R")
library(SnowballC)
library(stringr)
library(tm)
library(textir)
library(DAAG)
library(gamlr)
library(maptpx)

SPEECH[,5:7]<-""
allonewords<-""
alltwowords<-""
allthreewords<-""

for (i in 1:nrow(SPEECH)){
  #PUTS ALL LETTERS IN LOWER CASE
  ex<-tolower(gsub("[[:punct:]]", "", SPEECH[i,2]))
  #REMOVES "STOP WORDS" ("THE", "IT", "AND", "OF", etc.)
  ex<-stripWhitespace(removeWords(ex, stopwords("english")))
  #mikephrase("this is a test of my phrase chunking system")$twowords
  #SELF-IDENTIFYING WORDS stopwords("english")[-(1:8)]
  
  #STEMS THE REMAINING WORDS
  ex<-paste(stemexcept((strsplit(ex, split=" ")[[1]])), collapse=" ")
  ex2<-paste(apply(mikephrase(ex)$twowords,   1, function(x) paste0(x, collapse="")), collapse=" ")
  ex3<-paste(apply(mikephrase(ex)$threewords, 1, function(x) paste0(x, collapse="")), collapse=" ")
  #SAVES THE FORMATTED WORDS TO THE THIRD COLUMN
  SPEECH[i,5]<-ex
  SPEECH[i,6]<-ex2
  SPEECH[i,7]<-ex3
  
  #CREATE A LIST OF EVERY WORD IN EVERY DESCRIPTION
  allonewords<-c(allonewords,     unlist(strsplit(SPEECH[i,5], split=" ")))
  alltwowords<-c(alltwowords,     unlist(strsplit(SPEECH[i,6], split=" ")))
  allthreewords<-c(allthreewords, unlist(strsplit(SPEECH[i,7], split=" ")))
  print(i)}

# names(SPEECH)[names(SPEECH) == 'Passign'] <- 'Party'
SPEEC$Party=SPEECH$Passign
