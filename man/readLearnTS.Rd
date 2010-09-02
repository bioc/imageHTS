\name{readLearnTS}

\alias{readLearnTS}
\alias{predictCellLabels}

\title{Learn, classify and predict cell labels.}

\description{
  \code{readLearnTS} trains an SVM classifier using cell features and
  a training cell set. \code{predictCellLabels} predicts cell labels.
}

\usage{
readLearnTS(x, featurePar, trainingSet, access='cache', cost, gamma)
predictCellLabels(x, uname, access='cache')
}

\arguments{
  \item{x}{An imageHTS object.}
  
  \item{uname}{A character vector, containing the well names to
    segment. See \code{getUnames} for details.}
  
  \item{featurePar}{A character string, indicating the filename
    containing the feature parameters.}

  \item{trainingSet}{A character string, indicating the filename
    containing the training cell set. See Details.}
  
  \item{access}{A character string indicating how to access the
    data. Valid values are \code{local}, \code{server} and \code{cache},
    the default. See \code{fileHTS} for details.}

  \item{cost}{An optional numeric vector containing the SVM costs to be
    explored during the cross-validation parameter grid-search. Default
    is \code{c(0.1, 1, 10, 20)}.}
  
  \item{gamma}{An optional numeric vector containing the radial kernel
    gamma parameters to be explored during the cross-validation
    parameter grid-search. Default is \code{c(0.0001, 0.001, 0.01, 0.1)}.}
}

\value{
  Returns an invisible list which contains: \code{classifier}, the
  trained classifier obtained by \code{tune.svm} and \code{cft}, the
  features that were used to train the classifier.
}

\details{
  \code{readLearnTS} trains an SVM classifier using cell features and
  a training cell set. Features enumerated in the
  \code{remove.classification.features} field of the feature parameters
  are not considered for classification. The training set, pointed by \code{trainingSet},
  is a tab-separated file containing the rows \code{uname}, \code{spot},
  \code{id} and \code{label}. Each row designates a cell. This file is
  constructed by using the output of the cellPicker module, see
  \code{popCellPicker}. After completion, \code{readLearnTS} writes the
  a RDA file \'data/classifier.rda\' in the local project
  directory. This file contains the list returned by \code{readLearnTS}.

  \code{predictCellLabels} uses the trained classifier located in the
  file \'data/classifier.rda\' and cell features to predict cell labels
  of wells indicated by \code{uname}. For each well, the function
  writes the file \code{clabels}, which contains the predicted cell
  labels.

  If present, \code{popCellPicker} shows the predicted cell
  labels. Several iterations of \code{readLearnTS},
  \code{predictCellLabels} and \code{popCellPicker} calls are useful to
  build an efficient cell classifier.
}

\seealso{
  \code{\link{popCellPicker}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## see vignette for details
}
