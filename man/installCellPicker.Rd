\name{installCellPicker}

\alias{installCellPicker}
\alias{installWebQuery}

\title{Set up imageHTS web modules}

\description{
  \code{installCellPicker} and \code{installWebQuery} install the
  cellPicker and webQuery modules in the local project directory.

  \code{cellPicker} is a web application that allows the interactive
  selection/annotation of cells within images using a point-and-click web
  interface.

  \code{webQuery} is a web application that allows to query
  information from the screen by well and well annotation. The 
  webQuery module requires a web server and PHP to be installed.
}

\usage{
installCellPicker(x)
installWebQuery(x)

}

\arguments{
  \item{x}{An imageHTS object.}
}

\value{
  None.
}

\details{
  After installation, the cellPicker module can be used with the
  function \code{popCellPicker}.
  
  \code{x} must be annotated using \code{annotate} before using
  \code{installWebQuery}. After installation, the webQuery module
  can be used with the function \code{popWebQuery}.
}

\seealso{
  \code{\link{popCellPicker}}, \code{\link{popWebQuery}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## initialize imageHTS object using the local submorph screen
local = tempdir()
server = system.file('submorph', package='imageHTS')
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
x = annotate(x, 'conf/annotation.txt')

installCellPicker(x)
installWebQuery(x)
}


