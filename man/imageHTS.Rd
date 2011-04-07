\name{imageHTS}

\alias{imageHTS}
\alias{imageHTS-class}
\docType{package}

\title{Package overview}

\description{
  imageHTS is an R package dedicated to the analysis of high-throughput
  microscopy-based screens. The package provides a modular and
  extensible framework to segment cells, extract quantitative cell features,
  predict cell types and browse screen data through web interfaces. Designed
  to operate in distributed environments, imageHTS provides a standardized
  access to remote screen data, facilitating the dissemination of
  high-throughput microscopy-based screens.
}

\section{Package content}{
  The following function instantiates the imageHTS object.
  \itemize{
  \item parseImageConf: instantiate an imageHTS object from a local
    or remote screen data repository
  }
  
 The following functions process, segment, quantify, summarize
 the well images.
 \itemize{
 \item segmentWells: segment cells in well images
 \item extractFeatures: extract cell features from segmented images
 \item readLearnTS: train a cell classifier
 \item predictCellLabels: predict cell labels
 \item summarizeWells: summarize cell populations
 }
 
 The following functions provides means to display and inspect the
 screen data.
 \itemize{
 \item installWebQuery: install the webQuery module
 \item popWebQuery: pop the webQuery module
 \item installCellPicker: install the cellPicker module
 \item popCellPicker: pop the cellPicker module
 \item segmentATH: segment cells stained for DNA, actin and tubulin 
 \item getCellFtrsATH: extract features from cells stained for DNA, actin and tubulin 
 }
 
 The following functions give access to the screen data.
 \itemize{
 \item fileHTS: build the path to a screen data file
 \item readHTS: read a screen data file
 \item parseDCF: read a DCF configuration file
 \item collectCellFeatures: collect cell features
 \item getWellFeatures: get well metadata, features and annotation information 
 }
 
 The following manipulate well unique names.
 \itemize{
 \item getUnames: get well unique names
 \item prw2uname: convert a (plate, replicate, well) data frame in well unique names
 \item uname2prw: convert well unique names in a (plate, replicate, well) data frame
 \item rowcol2well: convert a (row, col) data frame in well names
 \item well2rowcol: convert well names in a (row, col) data frame
 \item well2wellid: convert well coordinates in numerical well identifiers
 }
 
 Miscellaneous functions.
 \itemize{
 \item zprime: compute the Z'-factor quality score  
 \item highlightSegmentation: highlight segmented objects in an image
 \item countObjects: count the number of objects in a segmented image
 \item getImageConf: get the imageHTS configuration
 }
}

\section{Authors}{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\keyword{package}


