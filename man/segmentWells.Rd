\name{segmentWells}

\alias{segmentWells}

\title{Segment cells in well images}

\description{
  Segment cells in well images.
}

\usage{
segmentWells(x, uname, segmentationPar, access='cache', writeData=TRUE)
}

\arguments{
  \item{x}{An imageHTS object.}
  
  \item{uname}{A character vector, containing the well names to
    segment. See \code{getUnames} for details.}
  
  \item{segmentationPar}{A character string, indicating the filename
    containing the segmentation parameters.}
  
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}

  \item{writeData}{A boolean indicating whether the segmentation
    data should be written to the project directory. Default is
    \code{TRUE}.}
}

\value{
  If \code{uname} is of length 1, returns an invisible list
  containing: \code{cal}, the calibrated image; \code{nseg}, the
  nucleus mask and \code{cseg}, the cell mask.
}

\details{
  \code{segmentWells} reads the DCF segmentation parameters file pointed by
  \code{segmentationPar}. The file must contain the core segmentation
  function name indicated in the \code{seg.method} field. For each well
  indicated by \code{uname}, \code{segmentWells} calls the core
  segmentation function which returns a list containing a list of three
  \code{EBImage} images: \code{cal}, the calibrated image; \code{nseg}, the
  nucleus mask and \code{cseg}, the cell mask. See \code{segmentATH} for
  an example of a core segmentation function.

  If \code{writeData} is \code{TRUE}, \code{segmentWells} writes for
  each well indicated by \code{uname}: the calibrated image data \code{cal},
  the segmentation data \code{seg}, a calibrated JPEG image
  \code{viewfull}, untiled JPEG images \code{viewunmonted}, a
  calibrated JPEG image with segmentation annotation \code{viewseg}, a
  thumbnail JPEG image \code{viewthumb} and Javascript segmentation
  contour information \code{contour}. See \code{fileHTS} for details
  about these files.
}

\seealso{
  \code{\link{segmentATH}}, \code{\link{getUnames}}, \code{\link{fileHTS}}
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

## segment one well
uname = getUnames(x, content='rluc')[1]
z = segmentWells(x, uname=uname, segmentationPar='conf/segmentationpar.txt', writeData=FALSE)
if (interactive()) {
  seg = highlightSegmentation(z$cal, z$nseg, z$cseg)
  display(seg)
}
}
