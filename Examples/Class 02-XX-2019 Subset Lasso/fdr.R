
# extract p-value cutoff for E[fdf] < q
fdr_cut <- function(pvals, q){
  pvals <- pvals[!is.na(pvals)]
  n <- length(pvals)
  
  # Benjamini + Hochberg algorithm  
  j <- rank(pvals, ties.method="min") # rank pvals from smallest to largest
  sig <- pvals <= q*j/n               # draw a line with slope q/N 
  sig[pvals<max(pvals[sig])] <- TRUE  # find max point where pval cross the line, this is rejection region
  o <- order(pvals)
  
  plot(pvals[o], log="xy", col=c("grey60","red")[sig[o]+1], pch=20, 
       ylab="p-values", xlab="tests ordered by p-value", main = paste('FDR =',q))
  lines(1:n, q*(1:n)/n)
  
  return(max(pvals[sig]))
}

