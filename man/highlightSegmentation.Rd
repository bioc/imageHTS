\name{highlightSegmentation}

\alias{highlightSegmentation}

\title{Highlight segmented objects in an image}

\description{
  Highlight segmented objects in an image.
}

\usage{
  highlightSegmentation(cal, nseg=NULL, cseg=NULL, thick=FALSE, opac=1, col=c('#ffff00', '#ff00ff'), border=FALSE, toRGB=TRUE) 
}

\arguments{
\item{cal}{An EBImage \code{Image} object containing the original image.}
\item{nseg}{An optional EBImage \code{Image} object containing the nucleus mask.}
\item{cseg}{An optional EBImage \code{Image} object containing the cell mask.}
\item{thick}{A logical indicating whether to use thick borders (useful for print). Default is \code{FALSE}.}
\item{opac}{A numeric of the opacity value for drawing object contours; can be either a single numeric value or a numeric vector containing two opacity values, the first one for nuclei and the second one for cell bodies. Opacity ranges from 0 to 1, with 0 being fully transparent and 1 fully opaque}
\item{col}{A character vector of two R colors for drawing contours of nuclei and cell bodies. By default, nuclei are highlighted in \code{yellow} and cells are highlighted in \code{magenta}.}
\item{border}{A logical indicating whether contours should be closed at borders or not. Default is \code{FALSE}.}
\item{toRGB}{A logical indicating whether Grayscale images should be promoted to Color ones or not. Default is \code{TRUE}.}
}

\value{
  An EBImage image containing the annotated image.
}

\details{
  Highlights nuclei and cells by drawing contours of their boundaries. The colors, opacity, and border thicknes can be customized via the \code{col}, \code{opacity}, and \code{thick} arguments, repsectively.
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
