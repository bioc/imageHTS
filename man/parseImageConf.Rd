\name{parseImageConf}

\alias{parseImageConf}

\title{Instantiate an imageHTS object}

\description{
  Instantiate an imageHTS object from a local or remote screen data
  repository.
}

\usage{
parseImageConf(filename, localPath='myscreen', serverURL, access='cache')
}

\arguments{
  \item{filename}{A character string containing the name of the
    imageHTS configuration file. See Details.}
  
  \item{localPath}{A character string indicating the path to the
    project directory that contains (or will contain) the screen data.
    The directory must be writable and will be created if missing.
    All intermediate files created by imageHTS will be stored in this
    directory. Default is \code{myscreen}.}
  
  \item{serverURL}{An optional character string indicating the URL or
    path to the server from where to download the data, if not available
    in the local path.}
  
  \item{access}{A character string indicating how to access the
      data. Valid values are \code{cache} (the default), \code{local} and
      \code{server}. See \code{fileHTS} for details.}
}

\value{
  An \code{imageHTS} object.
}

\details{
  \code{parseImageConf} gets access to the configuration file depending
  on the value of the \code{access} argument. If \code{local}, the file
  is loaded from the local path; if
  \code{server}, from the server and if \code{cache}
  \code{parseImageConf} tries to first load the file from the local
  path, and if not present, downloads it from the server.
  This dual repository feature is useful
  when screen images are stored in a different location from where they
  are analysed.
  See \code{fileHTS} for details.
  
  The imageHTS configuration file is a DCF file which contains the
  following fields:
  \itemize{
    \item AssayName: a character string containing the name of the assay
    \item SourceFilenamePattern: a character string containing the
    source image pattern path, relative to the local path. Special
    fields: \{plate\}, \{replicate\}, \{row\}, \{col\} and \{channel\}
    will be replaced by elements of the corresponding fields
    \code{PlateNames}, \code{ReplicateNames}, \code{RowNames},
    \code{ColNames} and \code{ChannelNames}.
    \item PlateNames: a comma-separated character vector, containing the
    plate names, to replaced in the field \{plate\} of \code{SourceFilenamePattern}.
    \item ReplicateNames: same as \code{PlateNames} for the field \{replicate\}.
    \item RowNames: same as \code{PlateNames} for the field \{row\}.
    \item ColNames: same as \code{PlateNames} for the field \{col\}.
    \item ChannelNames: same as \code{PlateNames} for the field \{channel\}.
    \item Montage: an optional comma-separated vector of two
    integers. If source images contain assembled different spot images of
    the well, this vector contains the dimension of the montage.
  }

  Use the command \code{getImageConf} to retrieve the configuration file
  from an \code{imageHTS} object.
}

\seealso{
  \code{\link{imageHTS}}, \code{\link{fileHTS}}, \code{\link{configure}}, \code{\link{getImageConf}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
local = tempdir()
server = system.file('submorph', package='imageHTS')
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
getImageConf(x)
}

