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