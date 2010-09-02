\name{getImageConf}

\alias{getImageConf}

\title{Get the imageHTS configuration}

\description{
  Get the imageHTS configuration from an imageHTS object.
}

\usage{
getImageConf(x)
}

\arguments{
  \item{x}{An imageHTS object.}
}

\value{
  A list containing the imageHTS configuration attributes.
}

\details{
  See the documentation of \code{\link{parseImageConf}} for details.
}

\seealso{
  \code{\link{parseImageConf}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
local = tempdir()
server = system.file('submorph', package='imageHTS')
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
getImageConf(x)
}


