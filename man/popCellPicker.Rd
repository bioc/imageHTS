\name{popCellPicker}

\alias{popCellPicker}
\alias{popWebQuery}

\title{Pop up imageHTS web modules}

\description{
  Pop up the imageHTS web modules cellPicker and webQuery, using
  the web browser.
}

\usage{
popCellPicker(x, uname, spot, id.highlight, access='server', browse=TRUE)
popWebQuery(x, access='server', browse=TRUE)
}

\arguments{
  \item{x}{An imageHTS object.}
  \item{uname}{A character vector, containing the well names to
    annotate. See \code{getUnames} for details.}
  \item{spot}{An optional numeric vector, containing the spot indexes of
    the wells to annotate. If missing, only the first spot is used.}
  \item{id.highlight}{An optional numeric vector, containing the object indexes
    to highlight. If missing, no object is highlighted.}
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{cache} and \code{server},
    the default. See \code{fileHTS} for details.}
  \item{browse}{A logical indicating whether the web browser should be
    loaded. Default is \code{TRUE}.}
}

\value{
  A character string containing the URL to access the cellPicker or the
  webQuery web module.
}

\details{
  cellPicker must be installed using \code{installCellPicker} before
  using \code{popCellPicker} on the local project directory. If present,
  the numeric vector \code{spot} must have the same length as
  \code{uname}.

  webQuery must be installed using \code{installWebQuery} before
  using \code{popWebQuery} on the local project directory.
}

\seealso{
  \code{\link{installCellPicker}}, \code{\link{installWebQuery}}, \code{\link{getUnames}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## initialize imageHTS object using the remote kimorph screen
local = tempdir()
server = 'http://www.huber.embl.de/cellmorph/kimorph/'
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)

if (interactive()) {
  popCellPicker(x, uname=c('002-02-A11', '001-01-C17'))
  popWebQuery(x)
}
}


