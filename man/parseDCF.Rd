\name{parseDCF}

\alias{parseDCF}

\title{Parse a DCF file}

\description{
  Parse a DCF file.
}

\usage{
parseDCF(filename)
}

\arguments{
  \item{filename}{A character string containing the path to a DCF file.}
}

\value{
  A list of character vector, containing the DCF read fields.
}

\details{
  The DCF format is a simple text format where each line is a field of
  the form \'a: a_0, a_1, ..., a_n\': \code{a} is the field name and
  \code{ak} the k-th value of \code{a}. Field name is separated from
  values by a colon \':\'. Field values are separated from each other
  by a comma \',\'.

  \code{readHTS} with format \code{dcf} is a higher-level function that
  parses a specific DCF file from a given imageHTS project.
}

\seealso{
  \code{\link{readHTS}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
imageconf = system.file('submorph/conf/imageconf.txt', package='imageHTS')
parseDCF(imageconf)
}

