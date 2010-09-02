\name{getWellFeatures}

\alias{getWellFeatures}

\title{Get well metadata, features and annotation information}

\description{
  Get well metadata, features and annotation information.
}

\usage{
  getWellFeatures(x, uname, feature=TRUE)
}

\arguments{
\item{x}{An imageHTS object.}
\item{uname}{a character vector, containing well names.
    See \code{getUnames} for details.}
  
\item{feature}{A character vectors containing the requested features.
    Default is \code{TRUE}, returning all well features.}
}

\value{
  Returns a data frame containing well features.
}

\details{
  \code{getWellFeatures} return \code{fData}, the well features
  loaded during the \code{configure} and \code{annotate} steps.
  Features include: \code{controlStatus}, the well content status
  derived from \'plateconf.txt\'.
}

\seealso{
  \code{\link{fData}}, \code{\link{configure}}, \code{\link{annotate}}
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
x = annotate(x, 'conf/annotation.txt')

## select non-empty wells
unames = setdiff(getUnames(x), getUnames(x, content='empty'))
getWellFeatures(x, unames)

}
