# OOS
source('speechmatch.R')
source('ocmatch.R')
source('naref.r')
dim(textmatch)
dim(SPEECH)

YYoos<-OCdata$prefOC
YYis<-c()
YYis[which(as.character(SPEECH$Party)=="D")]<-1
YYis[which(as.character(SPEECH$Party)=="R")]<--1
OOS <- DocumentTermMatrix(Corpus(VectorSource(textmatch[,4]))) #3 is 1-word
IS <- DocumentTermMatrix(Corpus(VectorSource(SPEECH[,7])))   #5 is 1-word
OOSm <- removeSparseTerms(OOS, sparse= 0.991); dim(OOSm)
ISm <- removeSparseTerms(IS, sparse= 0.995); dim(ISm)
ALL<-c(ISm, OOSm, recursive=T)
TRAIN=1:nrow(ISm)

rowTotals <- apply(ALL[-TRAIN,] , 1, sum) #Find the sum of words in each Document
remove <- which(rowTotals==0); length(remove)

# [4a] BUILD TOPIC MODEL  
tpcs <- topics(ALL[TRAIN,],K=5*(1:5),tol=10)  
summary(tpcs, n=10)   
treg<-glm(as.factor(YYis) ~ tpcs$omega, family="binomial")
tpcs.oos <- topics(ALL[-TRAIN,],K=5*(1:5),tol=10)  
summary(tpcs.oos, n=10)   
ptreg <- predict(treg, newdata=as.data.frame(tpcs.oos$omega), type="response")
plot(ptreg ~ YYoos[-remove])

#MNIR
dev.off()
cl <- makeCluster(detectCores())
dim(ISm)
fitCS <- mnlm(cl, YYis, ISm, bins=5,gamma=1)
B <- coef(fitCS)
print(names(B[2,order(B[2,])[1:10]]))
print(names(B[2,order(-B[2,])[1:10]]))
z <- srproj(B,as.matrix(ISm))
print(summary(fwd <- glm(as.factor(YYis) ~ z, family="binomial")))

par(mfrow=c(1,2),cex=1.5)
#PLOT THE LINEAR MODEL        
plot(fwd$fitted ~ factor(YYis), 
     varwidth=TRUE, col=c("red","navy"), pch=20,
     xlab="Partisanship R(-1) D(1)", ylab="predicted probability of partisanship", 
     main=("Glm fit from MNIR (IS)"))
#PLOT Z VS M(PHRASE LENGTH)
plot(z, pch=20, main=("SR"),
     ylab="number of phrases", xlab="z")  








summary(fwd <- lm(YY ~ z)) 
plot(fwd$fitted ~ factor(YY), 
     varwidth=TRUE, col="lightslategrey")

plot(z, pch=21, main="SR projections")

# stopCluster(cl)