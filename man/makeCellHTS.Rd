\name{makeCellHTS}

\alias{makeCellHTS}

\title{Segmentation of yeast cells and ring-shaped objects.}

\description{
  \code{makeCellHTS} creates a cellHTS2 object.
}

\usage{
makeCellHTS(x, profiles, measurementNames, name)
}

\arguments{
  \item{x}{An imageHTS object.}

  \item{profiles}{A data frame containing the phenotypic profiles. See Details.}
  
  \item{measurementNames}{An optional character vector containing the
    measurement names. If missing, column names of \code{profiles} are used.}
  
  \item{name}{An optional character string containing the name of the
    assay.} 
}

\value{
  Returns a \code{cellHTS2} object.
}

\details{
  \code{profiles} is a data frame containing the phenotypic profiles,
  usually returned by \code{summarizeWells} or \code{readHTS}. Since
  cellHTS2 cannot handle large report, the dimension of the profiles
  must be lower than 10. This is usually done by subsetting columns or
  by dimension reduction.
}

\seealso{
  \code{\link{summarizeWells}}, \code{\link{installWebQuery}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
 \dontrun{
   ## initialize kimorph object
   localPath = file.path(tempdir(), 'kimorph')
   serverURL = 'http://www.huber.embl.de/cellmorph/kimorph/'
   x = parseImageConf('conf/imageconf.txt', localPath=localPath, serverURL=serverURL)
   x = configure(x, 'conf/description.txt', 'conf/plateconf.txt', 'conf/screenlog.txt')
   x = annotate(x, 'conf/annotation.txt')
   
   ## get profiles
   profiles = readHTS(x, type='file', filename='data/profiles.tab', format='tab')

   ## prepare cellHTS2 report
   ft = c('med.c.t.m.int', 'med.c.g.ss', 'med.c.g.ec', 'med.n.h.m.int', 'med.c.a.m.int')
   measurementNames = c('tubulin intensity', 'cell size', 'cell eccentricity', 'dna intensity', 'actin intensity')
   y = makeCellHTS(x, profiles[,c('uname', ft)], measurementNames=measurementNames, name='kimorph')
   pathConf = file.path(localPath, 'conf')
   y = configure(y, 'description.txt', 'plateconf.txt', 'screenlog.txt', path=pathConf)
   y = annotate(y, 'annotation.txt', path=pathConf)
   yn = normalizePlates(y, scale='multiplicative', log=FALSE,
   method='median', varianceAdjust='none')

   ## write cellHTS2 report
   se = getSettings()
   se$plateList$intensities$include = TRUE
   setSettings(se)
   writeReport(raw=y, normalized=yn, outdir='report-cellHTS2', force=TRUE)
  }
}
