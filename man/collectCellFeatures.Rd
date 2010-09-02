\name{collectCellFeatures}

\alias{collectCellFeatures}

\title{Collect cell features.}

\description{
  Collect cell features from a given set of cells.
}

\usage{
collectCellFeatures(x, uname, spot=NULL, id=NULL, access='cache') 
}

\arguments{
  \item{x}{An imageHTS object.}
  
  \item{uname}{a character vector, containing the well names from where to
    collect the cell features. See \code{getUnames} for details.}
  
  \item{spot}{An optional numeric vector indicating the spots from where to
    collect the cell features. If missing, all spots are considered. The
    length of the vector must be the same as \code{uname}.}
  
  \item{id}{An optional numeric vector indicating the cell ids. If
    missing, all cells are considered. The length of the vector must be
    the same as \code{uname}.}

  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}
}

\value{
  A data frame containing the cell features.
}

\details{
  Contrary to \code{readHTS}, \code{collectCellFeatures} collects cell
  features through multiple wells. Output data frame contains the columns
  \code{uname}, \code{spot} and \code{id}.
}

\seealso{
  \code{\link{extractFeatures}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## see extractFeatures for an example of collectCellFeatures
## example(extractFeatures)
}
