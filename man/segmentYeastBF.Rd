\name{segmentYeastBF}

\alias{segmentYeastBF}
\alias{segmentRing}

\title{Segmentation of yeast cells and ring-shaped objects.}

\description{
  \code{segmentYeastBF} segments yeast cells from bright field micrscopy
  images.
  \code{segmentRing} segments ring-shape objects in images.
}

\usage{
segmentYeastBF(x, uname, p, access)
segmentRing(a, p)
}

\arguments{
  \item{x}{An imageHTS object.}

  \item{uname}{A character string, containing the well name to segment.}

  \item{p}{A list of character vectors, containing the segmentation
    parameters. This is the output of \code{parseDCF}, given an input
    segmentation configuration file. See details.} 
  
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}
  
  \item{a}{An EBImage image object or a matrix containing the image to
    segment.}
}

\value{
 \code{segmentYeastBF} returns a list containing three \code{EBImage}
 images: \code{cal}, the calibrated image; \code{nseg}, the nucleus
 mask and \code{cseg}, the cell mask.

 \code{segmentRing} returns a list containing two \code{EBImage}
 images: \code{nseg}, the nucleus mask and \code{cseg}, the cell mask.
}

\details{
  \code{segmentYeastBF} is a high-level segmentation function that
  can be specified in the \code{seg.method} field of a segmentation
  configuration file, called by the higher-level function
  \code{segmentWells}.
  
  \code{segmentRing} is used by \code{segmentYeastBF} and segments
  images containing ring-shaped objects. The list of parameters \code{p}
  should contain:
  \itemize{
    \item \code{edge.threshold}: a threshold parameter giving the cell edges
    \item \code{max.membrane.thickness}: the maximum membrane thickness,
    in pixels
    \item \code{crown.thickness}: the membrane thickness, in pixels
    \item \code{crown.steps}: a vector of 3 values, containing the
    minimum cell diameter, the maximum cell diameter, and the step between
    all possible diameters
    \item \code{locmin.threshold.width}: the adaptive threshold window
    width to compute the local minima, to call cell centers
    \item \code{locmin.threshold.offset}: the adaptive threshold window
    offset
    \item \code{locmin.erode.size}: the size of the erode paramter
    cleaning up the local minima map
    \item \code{cell.max.overlap}: the maximum cell overlap size, in pixels
    \item \code{nucleus.radius.offset}: the nucleus radius negative offset
    \item \code{cell.radius.offset}: the cell radius negative offset 
  }
}

\seealso{
  \code{\link{segmentWells}}, \code{\link{segmentATH}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
filename = system.file('yeast.jpeg', package='imageHTS')
a = readImage(filename)
if (interactive()) display(a)
p = list(edge.threshold=0.05, max.membrane.thickness=5, crown.thickness=8,
    crown.steps=c(31, 61, 4), locmin.threshold.width=9, locmin.threshold.offset=0.15, 
    locmin.erode.size=3, cell.max.overlap=2, nucleus.radius.offset=10,
    cell.radius.offset=4)
seg = segmentRing(a, p)
hseg = highlightSegmentation(EBImage::channel(a, 'rgb'), cseg=seg$cseg,
thick=TRUE)
if (interactive()) display(hseg)
}
