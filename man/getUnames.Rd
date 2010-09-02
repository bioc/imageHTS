\name{getUnames}

\alias{getUnames}

\title{Get well unique names}

\description{
  Build valid well unique names, according to the screen layout and
  to input filter arguments. Each well in the screen has an
  unique name, based on its plate, replicate, row and column indices.
}

\usage{
getUnames(x, plate, replicate, row, col, content)
}

\arguments{
  \item{x}{An imageHTS object.}
  \item{plate}{An optional numeric vector indicating the plate indices
    to keep.}
  \item{replicate}{An optional numeric vector indicating the replicate indices
    to keep.}
  \item{row}{An optional numeric vector indicating the row indices
    to keep.}
  \item{col}{An optional numeric vector indicating the column indices
    to keep.}
  \item{content}{An optional character string indicating the well
    content type to keep.}
}

\value{
  A character vector containing a set of well unique names that are
  compatible with the input filter arguments.
}

\details{
  Well \code{content} types are defined in the \code{plateconf.txt}
  configuration file. See \code{getWellFeatures} for details.

  Well unique names can be also built and manipulated using
  \code{prw2uname} and \code{uname2prw}.
}

\seealso{
  \code{\link{prw2uname}}, \code{\link{uname2prw}}, \code{\link{getWellFeatures}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## initialize imageHTS object using the local submorph screen
local = tempdir()
server = system.file('submorph', package='imageHTS')
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
x = configure(x, 'conf/description.txt', 'conf/plateconf.txt',
'conf/screenlog.txt')

getUnames(x, col=2)
getUnames(x, replicate=1, col=3)
getUnames(x, content='sample') ## get 'sample' wells
setdiff(getUnames(x), getUnames(x, content='empty')) ## get non-empty wells
}


