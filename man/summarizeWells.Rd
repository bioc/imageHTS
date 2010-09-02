\name{summarizeWells}

\alias{summarizeWells}

\title{Summarize cell features}

\description{
  Compute phenotypic profiles by summarizing cell population features.
}

\usage{
summarizeWells(x, uname, featurePar, access='cache')
}

\arguments{
  \item{x}{An imageHTS object.}
  
  \item{uname}{A character vector, containing the well names to
    summarize. See \code{getUnames} for details.}
  
  \item{featurePar}{A character string, indicating the filename
    containing the feature extraction parameters.}
  
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}
}

\value{
  A data frame, containing the phenotypic profiles.
}

\details{
  \code{summarizeWells} computes for each well, summary statistics
  about cell features. Currently, cell number \code{n} and
  median cell feature \code{med.*} (for each feature) are computed.

  Moreover, if the DCF segmentation parameters file pointed by
  \code{featurePar} includes the field \code{cell.classes}, containing
  a list of comma-separated cell classes, cell classes ratio are computed
  and included in the phenotypic profiles.

  \code{summarizeWells} creates the file \code{data/profiles.tab}
  which contains the phenotypic profiles. Use \code{readHTS} to
  read this file.
}

\seealso{
  \code{\link{extractFeatures}}, \code{\link{readHTS}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
  \dontrun{
    ## initialize imageHTS object using the local submorph screen
    local = tempdir()
    server = system.file('submorph', package='imageHTS')
    x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
    x = configure(x, 'conf/description.txt', 'conf/plateconf.txt', 'conf/screenlog.txt')
    
    ## segment non-empty wells
    unames = setdiff(getUnames(x), getUnames(x, content='empty'))
    segmentWells(x, uname=unames, segmentationPar='conf/segmentationpar.txt')
    
    ## extract features
    extractFeatures(x, uname=unames, 'conf/featurepar.txt')
    
    ## cell classification
    readLearnTS(x, 'conf/featurepar.txt', 'conf/trainingset.txt')
    predictCellLabels(x, unames)
    
    ## summarize features
    summarizeWells(x, unames, 'conf/featurepar.txt')
    
    ## get profiles
    profiles = readHTS(x, type='file', filename='data/profiles.tab', format='tab')
  }
}
