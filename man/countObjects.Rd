\name{countObjects}

\alias{countObjects}

\title{Count the number of objects in a segmented image}

\description{
  Count the number of objects in a segmented image.
}

\usage{
countObjects(cseg)
}

\arguments{
  \item{cseg}{An image containing the objects.}
}

\value{
  An integer indicating the number of objects in the cell mask.
}

\seealso{
  \code{\link{segmentWells}}
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
n = countObjects(z$cseg)
cat('number of cells=', n, '\n')
}

