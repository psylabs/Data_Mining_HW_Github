
## extract p-value cutoff for E[fdf] < q
fdr_cut <- function(pvals, q){
  pvals <- pvals[!is.na(pvals)]
  n <- length(pvals)
  
  j <- rank(pvals, ties.method="min")
  sig <- pvals <= q*j/n
  sig[pvals<max(pvals[sig])] <- TRUE
  
  o <- order(pvals)
  plot(pvals[o], log="xy", col=c("grey60","red")[factor(sig[o])], pch=20, 
       ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
  lines(1:n, q*(1:n)/n)
  
  return(max(pvals[sig]))
}

