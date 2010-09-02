\name{extractFeatures}

\alias{extractFeatures}

\title{Extract features from segmented images.}

\description{
  Extract features from segmented images.
}

\usage{
extractFeatures(x, uname, featurePar, access='cache')
}

\arguments{
  \item{x}{An imageHTS object.}
  
  \item{uname}{a character vector, containing the well names to
    segment. See \code{getUnames} for details.}
  
  \item{featurePar}{a character string, indicating the filename
    containing the feature extraction parameters.}
  
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}
}

\value{
  None.
}

\details{
  \code{extractFeatures} reads the DCF segmentation parameters file pointed by
  \code{featurePar}. The file must contain the field
  \code{extractfeatures.method} that indicates which core feature
  extraction function to use. The function takes two arguments:
  \code{cal}, the calibrated image to extract the features from, and
  \code{seg}, a list of two images, \code{cseg} and \code{nseg}, which
  contains the cell and nucleus segmentation masks, respectively. The
  function returns a matrix of features. See \code{getCellFtrsATH} for
  example.

  For each well, \code{extractFeatures} writes a \code{ftrs}
  segmentation data file that contains the cell features. Use
  \code{readHTS} or \code{collectCellFeatures} to get the cell features
  after extraction.
}

\seealso{
  \code{\link{getCellFtrsATH}}, \code{\link{fileHTS}}, \code{\link{collectCellFeatures}}
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
segmentWells(x, uname=uname, segmentationPar='conf/segmentationpar.txt')

## extract features from the well
extractFeatures(x, uname=uname, 'conf/featurepar.txt')

## read the feature file using readHTS
y = readHTS(x, type='ftrs', uname=uname)

## get features using collectCellFeatures
y = collectCellFeatures(x, uname=uname)
}
