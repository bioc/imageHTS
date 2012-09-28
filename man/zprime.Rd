\name{zprime}

\alias{zprime}

\title{Compute the Z'-factor quality score}

\description{
  Compute the Z'-factor quality score.
}

\usage{
  zprime(a, b, method=c('mahalanobis', 'robust', 'fixsd', 'original'))
}

\arguments{
\item{a, b}{Matrices of control features.}
  
\item{method}{a character vector, indicating which method should be
    used to compute the Z'-factor. Default is \code{mahalanobis}.
    See Details.}
}

\value{
  The Z'-factor, a numeric ranging from -infinity to 1.
}

\details{
  The Z'-factor is a popular metric measuring the separation of
  control features in high-throughput screens. The original paper
  describing the Z'-factor is Zhang, 1999, J Biomol Screen.

  Several univariate Z'-factor scores exist. The \code{original} Z'-factor
  from Zhang, 1999 is computed by {Z' = 1 - 3*(sd(a)+sd(b))/abs(mean(a)-mean(b))}.
  A more rigorous definition of the score, implemented by the method \code{fixsd}
  is given by {Z' = 1 - 3*sqrt(var(a)+var(b))/abs(mean(a)-mean(b))}, where the
  pooled standard deviation is computed by the square root of the sum of the control
  variances. A \code{robust} method, less sensitive to outliers, is computed by
  the relation {Z' = 1-3*(mad(a)+mad(b))/abs(median(a)-median(b))} where the
  control dispersions are computed with the mad and the control locations with the median.

  A multivariate extension of the Z'-factor score can be designed by
  linearly transforming the multivariate data to one dimension and
  computing the standard (here, \code{fixsd}) Z'-factor. It can be shown
  that the linear transform that maximizes the score is the LDA. Moreover,
  one can demonstrate that the resulting Z'-factor score is equivalent of
  computing {Z' = 1 - 3/dMaha(mu_a, mu_b, Sigma_a + Sigma_b)} where
  \code{dMaha} is the Mahalanobis distance.
}

\references{J. H. Zhang, T. D. Chung, K. R. Oldenburg. A Simple 
Statistical Parameter for Use in Evaluation and Validation of High
Throughput Screening Assays. J Biomol Screening, 1999.
}

\seealso{
  \code{\link{readHTS}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
  ## initialize imageHTS object using the local submorph screen
  local = tempdir()
  server = system.file('submorph', package='imageHTS')
  x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
  x = configure(x, 'conf/description.txt', 'conf/plateconf.txt', 'conf/screenlog.txt')
  
  ## get profiles
  profiles = readHTS(x, type='file', filename='data/profiles.tab', format='tab')
  a = profiles[match(getUnames(x, content='rluc'), profiles$uname),]
  b = profiles[match(getUnames(x, content='ubc'), profiles$uname),]
  
  ## compute Z'-factor scores on some features
  ft = c('med.c.t.m.majoraxis')
  cat('Z\'-factor original=',  zprime(a[,ft], b[,ft], 'original'), 'fixsd=',  zprime(a[,ft], b[,ft], 'fixsd'), '\n') 
  
  ## multivariate Z'-factor
  ft = c('med.c.t.m.majoraxis', 'med.n.h.m.majoraxis', 'med.c.h.b.mean')
  cat('Z\'-factor mahalanobis=',  zprime(a[,ft], b[,ft], 'mahalanobis'), '\n') 
}
