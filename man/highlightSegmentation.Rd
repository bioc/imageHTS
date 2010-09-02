\name{highlightSegmentation}

\alias{highlightSegmentation}

\title{Highlight segmented objects in an image}

\description{
  Highlight segmented objects in an image.
}

\usage{
  highlightSegmentation(cal, nseg=NULL, cseg=NULL, thick=FALSE)
}

\arguments{
\item{cal}{An EBImage image object containing the original image.}
\item{nseg}{An optional EBImage image object containing the nucleus mask.}
\item{cseg}{An optional EBImage image object containing the cell mask.}
\item{thick}{A logical indicating whether thick borders (useful for print)
    are required. Default is \code{FALSE}.}
}

\value{
  An EBImage image containing the annotated image.
}

\details{
  \code{highlightSegmentation} highlights nuclei and cells by outlining
  them in yellow and magenta.
}

\seealso{
  \code{\link{segmentWells}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## see segmentWells for an example of highlightSegmentation
## example(segmentWells)
}
