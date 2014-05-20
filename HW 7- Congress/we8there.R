## we8there

library(textir) # to get the data
library(maptpx) # for the topics function

data(we8there)

#  upon what scale to measure `words' for k-means is unclear (the answer is
#  actually: don't use kmeans; use a multinomial mixture instead).
#  You could do log(1+f) instead, or something like tf-idf (google that)
#  but in absence of better guidance, I just look at scaled f


# takes time, because you're making it dense from sparse
fs <- scale(as.matrix( we8thereCounts/rowSums(we8thereCounts) ))

kmfs <- kmeans(fs,10)  

## both this and topic modelling takes a long time...
## you're fitting massively high dimensional models (K*ncol(x))
## there are approximate distributed algorithms out there
## `stochastic block descent', etc...
## for really big data, I think you just focus on subsamples, most common words, etc.
## all these methods find are the dominant sources of variation, 
## so those should be present in small subsamples

## interpretation: we can see the words with cluster centers
## highest above zero (these are in units of standard deviation of f)
print(apply(kmfs$centers,1,function(c) colnames(fs)[order(-c)[1:10]]))
## shows words that occur far more in these clusters than on average

## topic modelling.  Treat counts as actual counts!
## i.e., model them with a multinomial
## we'll use the topics function in maptpx (there are other options out there)

## you need to convert from a Matrix to a `slam' simple_triplet_matrix
## luckily, this is easy.
x <- as.simple_triplet_matrix(we8thereCounts)

# to fit, just give it the counts, number of `topics' K, and any other args
tpc <- topics(x,K=10,tol=10) 
# note: tol is `tolerance threshold on minimization of deviance'
# the default is small, and not dependent upon sample size or dimension, 
# so it can lead to topics optimization taking forever.  
# You'll get a fine rough answer at higher tolerances.

## choosing the number of topics
## If you supply a vector of topic sizes, it uses a Bayes factor to choose
## (BF is like exp(-BIC), so you choose the bigggest BF)
## the algo stops if BF drops twice in a row
tpcs <- topics(x,K=5*(1:5),tol=10) # it chooses 10 topics 

## interpretation
# summary prints the top `n' words for each topic,
# under ordering by `topic over aggregate' lift:
#    the topic word prob over marginal word prob.
summary(tpcs, n=10) 
# this will promote rare words that with high in-topic prob

# alternatively, you can look at words ordered by simple in-topic prob
## the topic-term probability matrix is called 'theta', 
## and each column is a topic
## we can use these to rank terms by probability within topics
rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpcs$theta)[order(tpcs$theta[,2], decreasing=TRUE)[1:10]]


## Wordles!
## use the wordcloud library to plot a few
library(wordcloud)
## we'll size the word proportional to its in-topic probability
## and only show those with > 0.004 omega
## (it will still likely warn that it couldn't fit everything)
par(mfrow=c(1,2))
wordcloud(row.names(tpcs$theta), 
	freq=tpcs$theta[,1], min.freq=0.004, col="maroon")
wordcloud(row.names(tpcs$theta), 
	freq=tpcs$theta[,2], min.freq=0.004, col="navy")

## interpret the relationship between topics and overall rating
library(gamlr)
## omega is the n x K matrix of document topic weights
## i.e., how much of each doc is from each topic
## we'll regress overall rating onto it
stars <- we8thereRatings[,"Overall"]
tpcreg <- gamlr(tpcs$omega, stars)
# number of stars up or down for moving up 10\% weight in that topic
drop(coef(tpcreg))*0.1

regtopics.cv <- cv.gamlr(tpcs$omega, stars)
## give it the word %s as inputs
x <- 100*we8thereCounts/rowSums(we8thereCounts)
regwords.cv <- cv.gamlr(x, stars)

par(mfrow=c(1,2))
plot(regtopics.cv)
mtext("topic regression", font=2, line=2)
plot(regwords.cv)
mtext("bigram regression", font=2, line=2)

# max OOS R^2s
max(1-regtopics.cv$cvm/regtopics.cv$cvm[1])
max(1-regwords.cv$cvm/regwords.cv$cvm[1])



