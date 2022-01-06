\name{fileHTS}

\alias{fileHTS}
\alias{readHTS}

\title{Get access to screen data files}

\description{
  \code{fileHTS} builds the path or URL pointing to a screen data file.
  \code{readHTS} reads the file pointed by \code{fileHTS}.
}

\usage{
  fileHTS(x, type, ..., createPath=FALSE, access='cache')
  readHTS(x, type, ..., access='cache', format=NULL)
}

\arguments{
\item{x}{An imageHTS object.}
\item{type}{A character vector, indicating the requested file type. See Details.}
\item{...}{Optional arguments. See Details.}
\item{createPath}{A logical value specifying if the directories along the path should be created. Default is \code{FALSE}.}
\item{access}{A character string indicating how to access the
  data. Valid values are \code{cache} (the default), \code{local} and
  \code{server}. See Details.}
\item{format}{An optional character string indicating the format of the
  designated file. Valid formats are \code{tab}, a tab-separated file
  with headers; \code{rda}, a R data file; \code{dcf}, an imageHTS
  DCF configuration file.}
}

\value{
  \code{fileHTS} returns a character vector containing the path or URL of the
  requested file. \code{readHTS} returns the content of the requested file.
}

\details{
  In imageHTS, all screen data files are accessed through
  the function \code{fileHTS}. Screen data can be accessed at two locations: the
  \code{localPath}, which is local writable directory, and \code{serverURL},
  which is a server URL. \code{localPath} and \code{serverURL} are set during the
  instantiation of the \code{imageHTS} object with \code{parseImageConf}.
  
  If \code{access} equals \code{local}, \code{fileHTS} returns a local
  path pointing to the requested file. If \code{server}, \code{fileHTS}
  returns a server URL pointing to the requested file.
  If \code{cache} (the default), \code{fileHTS} tests if the file is
  present at the local path. If not, \code{fileHTS} tries to download
  the file from the server and copies it in the local path.
  \code{fileHTS} always returns a local path if \code{access} equals
  \code{cache}.

  A file is designated by its \code{type} and optional arguments. Known
  file types are:
  \itemize{
    \item \code{file}: general-purpose file. The character string
    \code{filename} indicates its path, relative to the project directory.
    File format is unspecified.
    
    \item \code{source}: source image, designated by the character
    string \code{uname} and the numeric \code{channel}.
    File format is unspecified.
    
    \item \code{cal}: calibrated image, designated by the character
    string \code{uname}. The file is a R data file which contains an
    \code{EBImage} image object.
    
    \item \code{seg}: segmentation data, designated by the character
    string \code{uname}. The file is a R data file which
    contains a list of two \code{EBImage} image objects: \code{cseg}
    containing the cell mask and \code{nseg} the nucleus mask.
    
    \item \code{ftrs}: cell features, designated by the character string
    \code{uname}. The file is a tab-separated file which contains the cell
    features.
    
    \item \code{clabels}: cell labels, designated by the character string
    \code{uname}. The file is a tab-separated file which contains the cell
    labels.
    
    \item \code{viewfull}: calibrated JPEG image, designated by the
    character string \code{uname}. The file is a JPEG image.
    
    \item \code{viewunmonted}: spot-untiled calibrated JPEG image, designated by the
    character string \code{uname} and the spot number \code{spot}. The
    file is a JPEG image.
    
    \item \code{viewseg}: segmented JPEG image, designated by the
    character string \code{uname}. The file is a JPEG image.
    
    \item \code{viewthumb}: thumbnail JPEG image, designated by the
    character string \code{uname}. The file is a JPEG image.
  }

  \code{readHTS} reads and returns the corresponding file. \code{format}
  must be specified if \code{type} is \code{file} or \code{source}. 
}

\seealso{
  \code{\link{parseImageConf}}, \code{\link{getUnames}}
}

\author{
  Gregoire Pau, \email{gregoire.pau@embl.de}, 2010
}

\examples{
## initialize imageHTS object using the local submorph screen
local = tempdir()
server = system.file('submorph', package='imageHTS')
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)

## fileHTS and readHTS examples
fileHTS(x, 'file', filename='conf/imageconf.txt')
readHTS(x, 'file', filename='conf/featurepar.txt', format='dcf')

## initialize imageHTS object using the remote kimorph screen
local = tempdir()
server = 'http://www.huber.embl.de/cellmorph/kimorph/'
x = parseImageConf('conf/imageconf.txt', localPath=local, serverURL=server)
}
