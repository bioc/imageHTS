\name{prw2uname}

\alias{prw2uname}
\alias{uname2prw}
\alias{rowcol2well}
\alias{well2rowcol}
\alias{well2wellid}

\title{Convert and parse well unique names}

\description{
  Functions to convert and parse well unique names.
}

\usage{
prw2uname(plate, replicate, row, col, well)
uname2prw(uname)
rowcol2well(row, col)
well2rowcol(well)
well2wellid(row, col, direction='row', dim)
}

\arguments{
  \item{plate}{An numeric vector of plate indices or a list containing
    the numeric vectors \code{plate}, \code{replicate}, \code{row} and \code{col}.}
  \item{replicate}{A numeric vector of replicate indices.}
  \item{row}{A numeric vector of row indices.}
  \item{col}{A numeric vector of column indices}
  \item{well}{A character vector of well names.}
  \item{uname}{A character vector of well unique names.}
  \item{direction}{A character string containing the direction of the
    mapping. Valid values are \code{row} and \code{col}. Default is
    \code{row}, where the well identifier 2 points is mapped to well (1, 2)}.
  \item{dim}{A numeric vector of length two, containing the dimensions
    (number of rows, number of columns) of a plate}.
}

\value{
  \code{prw2uname} returns a character vector of well unique names.
  \code{uname2prw} returns a data frame containing the columns
  \code{plate}, \code{replicate}, \code{row} and \code{col}.
  \code{rowcol2well} returns a character vector of well names.
  \code{well2rowcol} returns a data.frame containing the numeric vectors
  \code{row} and \code{col}.
  \code{well2wellid} returns a numeric vector containing the well identifiers.
}

\details{
  In \code{prw2uname}, wells can be specified using the arguments \code{row} and
  \code{col} or using the argument \code{well}.
}

\seealso{
  \code{\link{getWellFeatures}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## prw2uname and uname2prw
u = prw2uname(plate=1:2, replicate=1, row=2, col=3:4)
print(u)
prw = uname2prw(u)
print(prw)
prw2uname(prw)

## rowcol2well and well2rowcol
w = rowcol2well(row=1:3, col=5:7)
print(w)
rc = well2rowcol(w)
print(rc)
}


