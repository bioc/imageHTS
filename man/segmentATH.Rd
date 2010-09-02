\name{segmentATH}

\alias{segmentATH}
\alias{getCellFtrsATH}

\title{Example segmentation and feature extraction functions.}

\description{
  \code{segmentATH} and \code{getCellFtrsATH} are segmentation and
  feature extraction functions designed for cell images stained for
  DNA, actin and tubulin.
}

\usage{
segmentATH(x, uname, p, access)
getCellFtrsATH(cal, seg)
}

\arguments{
  \item{x}{An imageHTS object.}

  \item{uname}{A character string, containing the well name to segment.}

  \item{p}{A list of character vectors, containing the segmentation
    parameters. This is the output of \code{parseDCF}, given an input
    segmentation configuration file.} 

  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}
  
  \item{cal}{An EBImage image object containing the calibrated image.}
  
  \item{seg}{A list of two EBImage image objects: \code{cseg}, the cell
    segmentation mask and \code{nseg}, the nucleus segmentation mask.}
}

\value{
 \code{segmentATH} returns a list containing three \code{EBImage}
 images: \code{cal}, the calibrated image; \code{nseg}, the  nucleus
 mask and \code{cseg}, the cell mask.

 \code{getCellFtrsATH} returns a data frame containing the cell features.
}

\details{
  \code{segmentATH} is a segmentation function that can be specified
  in the \code{seg.method} field of a segmentation configuration file,
  called by the higher-level function \code{segmentWells}.

  \code{getCellFtrsATH} is a feature extraction function that can be
  specified in the \code{extractfeatures.method} field of a feature
  configuration file, called by the higher-level function
  \code{extractFeatures}.
}

\seealso{
  \code{\link{parseDCF}}, \code{\link{segmentWells}}, \code{\link{extractFeatures}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## see segmentWells and extractFeatures
}
