zprime = function(a, b, method=c('mahalanobis', 'robust', 'fixsd', 'original')) {
  method = match.arg(method)
  
  if (method=='mahalanobis') {
    if (is.null(dim(a))) a = matrix(a, nc=1)
    if (is.null(dim(b))) b = matrix(b, nc=1)
    mua = apply(a, 2, mean)
    mub = apply(b, 2, mean)
    dm = try(mahalanobis(mua, mub, cov(a) + cov(b)))
    if (class(dm)=='try-error') NA
    else 1-3/sqrt(dm)
  }
  else {
    if (method=='robust') 1-3*(mad(a)+mad(b))/abs(median(a)-median(b))
    else if (method=='fixsd') 1-3*sqrt(var(a)+var(b))/abs(mean(a)-mean(b))
    else 1-3*(sd(a)+sd(b))/abs(mean(a)-mean(b))
  }
}
