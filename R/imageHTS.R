## imageHTS class definition
setClass('imageHTS', contains='cellHTS',
         representation(localPath="character",
                        serverURL="character",
                        imageConf="list"
                        ),
         prototype=prototype(localPath='',
           serverURL='',
           imageConf=list()
           )
         )

## extract fields "x1: y1\n x2: y2\n..." to  named list with names x_k and values y_k
## - yk value are splitted between commas
## - trailing whitespaces are removed
parseDCF = function(filename) {
  if (!file.exists(filename)) stop('parseDCF: cannot open file "',filename,'"',sep='')

  ## parse file
  tt = paste(readLines(filename, warn=FALSE), '')

  ## split first :
  z = regexpr(':',tt)
  z[z<0] = nchar(tt)[z<0]+1
  aa = matrix('', nr=length(tt), nc=2)
  aa[,1] = substr(tt, 1, z-1)
  aa[,2] = substr(tt, z+1, nchar(tt))
  tt = apply(aa, 1, as.list)
  
  tt[lapply(tt, length)<2] = NULL
  tt = lapply(tt, function(z) {z[[2]]=strsplit(z[[2]], ',')[[1]] ; z})
  tt = lapply(tt, lapply, function (z) {gsub("^ *| *$", "", z)})
  
  names(tt) = sapply(tt,'[[', 1)
  tt = sapply(tt, '[[', 2)
  as.list(tt)
}

## overload configure
setMethod('configure', signature("imageHTS"),
          function(object, descripFile, confFile, logFile, path='missing') {
            ## try to fetch descripFile, confFile and logFile
            access='cache'
            fileHTS(object, type='file', filename=descripFile, access=access)
            fileHTS(object, type='file', filename=confFile, access=access)
            fileHTS(object, type='file', filename=logFile, access=access)
            
            ## call mother method
            cellHTSconfigure = getMethod('configure', signature('cellHTS'))

            ## cellHTSconfigure files cannot contains path ; make a new path
            ddescripFile = dirname(descripFile)
            dconfFile = dirname(confFile)
            dlogFile = dirname(logFile)
            if (length(unique(c(ddescripFile, dconfFile, dlogFile)))!=1) {
              stop('descripFile, confFile and logFile must be in the same directory')
            }
            confdir = ddescripFile
            if (confdir!='.') path = file.path(object@localPath, confdir)
            else path = object@localPath
            cellHTSconfigure(object, descripFile, confFile, logFile, path=path)
          })

## overload annotate
setMethod("annotate", signature("imageHTS"),
          function(object, geneIDFile, path='missing') {
            ## try to fetch geneIDFile
            access='cache'
            fileHTS(object, type='file', filename=geneIDFile, access=access)

            ## call mother method
            cellHTSannotate = getMethod('annotate', signature('cellHTS'))

            ## cellHTSannotate file cannot contains path ; make a new path
            confdir = dirname(geneIDFile)
            if (confdir!='.') path = file.path(object@localPath, confdir)
            else path = object@localPath
            cellHTSannotate(object, geneIDFile, path=path)
          })

readHTS = function(x, type, ..., access='cache', format=NULL) {
  f = fileHTS(x, type=type, ..., access=access)
  z = gregexpr('\\.', f)[[1]]
  extension = substr(f, z[length(z)]+1, nchar(f))

  ## format is based on type
  if (is.null(format)) format = switch(type, cal='rda', seg='rda', ftrs='tab', clabels='tab')
  if (is.null(format)) stop('file format cannot be determined: please specify \'format\'')

  switch(format,
         'tab'=read.table(f, header=TRUE, sep='\t', stringsAsFactors=FALSE),
         'dcf'=parseDCF(f),
         'rda'=get(load(f))
         )
}

fileHTS = function(x, type, ..., createPath=FALSE, access='cache') {
  stopifnot(class(x)=='imageHTS')
  if (missing(type)) stop("argument 'type' is missing")
  if (!access%in%c('cache', 'local', 'server')) stop("'access' must be 'cache', 'local' or 'server'")
  
  args = list(...)
  nargs = names(args)

  if ('uname'%in%nargs) uname = args$uname
  if ('channel'%in%nargs) channel = args$channel
  if ('spot'%in%nargs) spot = args$spot
 
  ## if 'uname' is present, instantiate: plate, replicate, row, col, subdir
  if ('uname'%in%nargs) {
    z = uname2prw(uname)
    plate = z$plate
    replicate = z$replicate
    row = z$row
    col = z$col
    subdir = prw2uname(plate, replicate)
  }
 
  ## build filename and dir
  dir = NULL
  switch(type,
         'file' ={
           if ('filename'%in%nargs) filename = args$filename
           else stop("fileHTS: argument 'filename' is missing")
           dir = ''
         },
         'source' = {
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           if (!'channel'%in%nargs) stop("fileHTS: argument 'channel' is missing")
           
           ic = getImageConf(x)
           plate = ic$PlateNames[plate]
           replicate = ic$ReplicateNames[replicate]
           row = ic$RowNames[row]
           col = ic$ColNames[col]
           channel = ic$ChannelNames[channel]
           
           filename = ic$SourceFilenamePattern
           filename = gsub('\\{plate\\}', plate, filename)
           filename = gsub('\\{replicate\\}', replicate, filename)
           filename = gsub('\\{row\\}', row, filename)
           filename = gsub('\\{col\\}', col, filename)
           filename = gsub('\\{channel\\}', channel, filename)

           ## spot
           if (length(grep('\\{spot\\}', filename))==1) {
             if (!'spot'%in%nargs) stop("fileHTS: argument 'spot' is missing")
             filename = gsub('\\{spot\\}', ic$SpotNames[spot], filename)
           }
           
           dir = ''
         },
         'cal'={
           ## Calibrated images in rda format
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('data', subdir)
           filename = paste(uname, '_cal.rda', sep='') 
         },
         'seg'={
           ## Segmentation information in rda format
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('data', subdir)
           filename = paste(uname, '_seg.rda', sep='') 
         },
         'contour'={
           ## Cell contours information in Javascript
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           if (!'spot'%in%nargs) stop("fileHTS: argument 'spot' is missing")
           dir = file.path('cellpicker', subdir)
           filename = paste(uname, '-', spot, '_con.js', sep='') 
         },
         'ftrs'= {
           ## Cell features in tab format
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('data', subdir)
           filename = paste(uname, '_ftrs.tab', sep='') 
         },
         'clabels'= {
           ## Cell labels in rda format
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('data', subdir)
           filename = paste(uname, '_clabels.tab', sep='') 
         },
         'jsclabels'= {
           ##  Cell labels in Javascript
          if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
          if (!'spot'%in%nargs) stop("fileHTS: argument 'spot' is missing")
           dir = file.path('cellpicker', subdir)
           filename = paste(uname, '-', spot, '_clabels.js', sep='') 
         },
         'viewfull'={
           ## Calibrated images in JPEG format, for visualisation purposes
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir=file.path('view', subdir)
           filename=paste(uname, '_full.jpeg', sep='') 
         },
         'viewunmonted'={
           ## Unmonted calibrated images in JPEG format, for visualisation purposes
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           if (!'spot'%in%nargs) stop("fileHTS: argument 'spot' is missing")
           dir = file.path('view', subdir)
           filename = paste(uname, '-', spot, '_um.jpeg', sep='') 
         },
         'viewseg'={
           ## Calibrated images with segmentation information in JPEG format, for visualisation purposes
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('view', subdir)
           filename = paste(uname, '_seg.jpeg', sep='') 
         },
         'viewthumb'={
           ## Thumbnail calibrated images
           if (!'uname'%in%nargs) stop("fileHTS: argument 'uname' is missing")
           dir = file.path('view', subdir)
           filename = paste(uname, '_thumb.jpeg', sep='') 
         })
  if (is.null(dir)) stop("fileHTS: invalid type '", type, "'", sep='')
  
  ## build filepath
  localPath = x@localPath
  serverURL = x@serverURL
  
  ## try to download the file from serverURL, if required
  if (access=='cache') {
    if (serverURL!='') {
      flocal = fileHTS(x, type, ..., createPath=TRUE, access='local')
      ## download if the file doesn't exist locally or if access = 'download'
      if (!file.exists(flocal)) {
        fserver =  fileHTS(x, type, ..., access='server')
        
        ## special case for local files
        if (regexpr('file://', fserver)==1) {
          w = substr(fserver, 8, nchar(fserver))
          ok = try(suppressWarnings(file.copy(w, flocal, overwrite=TRUE)), silent=TRUE)
        } else {
          ok = try(suppressWarnings(download.file(URLencode(fserver), flocal, mode='wb')), silent=TRUE)
          ## download.file returns 0 for success
          if (ok==0) ok = TRUE
        }
        
        if (class(ok)=='try-error' || !ok || file.info(flocal)$size==0) {
          warning(paste('cannot download the file pointed by \"', fserver, '\" to the location \"', flocal, '\"', sep=''))
          unlink(flocal)
        }
      }
    }
    access = 'local'
  }
  
  switch(access,
         'local'={
           paths = ifelse(dir!='', file.path(localPath, dir, filename), file.path(localPath, filename))
           if (createPath) sapply(paths, function(z) {
             dz = dirname(z)
             if (!file.exists(dz)) dir.create(dz, recursive=TRUE)
           })
           paths
         },
         'server'={
           urls = ifelse(dir!='', paste(serverURL, dir, filename, sep='/'), paste(serverURL, filename, sep='/'))
           as.character(sapply(urls, URLencode))
           urls
         }
         )
}

## (plate, replicate, row, col) to internal uname
## or list(plate, replicate, row, col)
prw2uname = function(plate, replicate, row, col, well) {
  if (is.list(plate)) do.call(prw2uname, plate)
  else {
    if (length(plate)==0) return(character(0))
    pr = sprintf('%03d-%02d', plate, replicate)
    if (missing(well)) {
      if (missing(row)) return(pr)
      else well = rowcol2well(row, col)
    }
    paste(pr, well, sep='-')
  }
}

uname2prw = function(uname) {
  plate = as.numeric(substr(uname, 1, 3))
  replicate = as.numeric(substr(uname, 5, 6))
  row = as.numeric(sapply(substr(uname, 8, 8), charToRaw))-64
  col = as.numeric(substr(uname, 9, 10))
  data.frame(plate=plate, replicate=replicate, row=row, col=col)
}

rowcol2well = function(row, col) {
  sprintf('%s%02d', LETTERS[row], col)
}

well2rowcol = function(well) {
  data.frame(row=as.numeric(sapply(substr(well, 1, 1), charToRaw))-64,
       col=as.numeric(substr(well, 2, 3)))
}

getUnames = function(x, plate, replicate, row, col, content) {
  if (missing(x)) stop("'x' is missing")
  stopifnot(class(x)=='imageHTS')

  ic = getImageConf(x)
  nbPlates = max(fData(x)$plate)
  nbReplicates = length(ic$ReplicateNames)
  nbRows = pdim(x)[1]
  nbCols = pdim(x)[2]

  if (missing(plate)) plate = 1:nbPlates
  if (missing(replicate)) replicate = 1:nbReplicates
  if (missing(row)) row = 1:nbRows
  if (missing(col)) col = 1:nbCols

  lplate = length(plate)
  lreplicate = length(replicate)
  lrow = length(row)
  lcol = length(col)

  aplate = rep(plate, each=lreplicate*lrow*lcol)
  areplicate = rep(rep(replicate, each=lrow*lcol), lplate)
  arow = rep(rep(row, each=lcol), lplate*lreplicate)
  acol = rep(col, lplate*lreplicate*lrow)

  z = prw2uname(aplate, areplicate, arow, acol)

  ## filtering according to content (in plateconf.txt)
  if (!missing(content)) {
    cs = getWellFeatures(x, z, 'controlStatus')
    z = z[cs%in%content]
  }
 
  z
}

## display
displayHTS = function(x, unames, filename='display.html', width=NULL, seg=FALSE, access=NULL, browse=TRUE, thumb=FALSE, gene=FALSE, webquery=FALSE) {
  if (webquery) {
    filename = paste(fileHTS(x, 'webquery', url=TRUE, access='server'), '?search=', paste(unames, collapse='+'), sep='')
  }
  else {
    filename = file.path(x@path, filename)
    if (seg) {
      z = fileHTS(x, 'viewseg', uname=unames, url=TRUE, access=access)
      z = matrix(hwriteImage(z, link=z, table=FALSE, width=width), ncol=1)
      dimnames(z) = list(unames, 'segmented')
    } else if (thumb) {
      z = fileHTS(x, 'viewthumb', uname=unames, url=TRUE, access=access)
      zf = fileHTS(x, 'viewfull', uname=unames, url=TRUE, access=access)
      z = matrix(hwriteImage(z, link=zf, table=FALSE, width=width), ncol=1)
      dimnames(z) = list(unames, 'thumb')
    } else {
      nbspots = prod(getImageConf(x)$Montage)
      z = fileHTS(x, 'viewunmonted', uname=rep(unames,each=nbspots), spot=1:nbspots, url=TRUE, access=access)
      z = matrix(hwriteImage(z, link=z, table=FALSE, width=width), ncol=nbspots, byrow=TRUE)
      dimnames(z) = list(unames, 1:nbspots)
    }

    if (gene) z = cbind(gene=getWellFeatures(x, rownames(z), 'GeneID'), z)
  
    hwrite(z, filename, col.bgcolor='#ffffa0')
  }
 
  if (browse) {
    cat('Starting a Web browser on file=', filename,' ...\n', sep='') 
    browseURL(filename)
  }
               
  invisible(filename)
}

## uname can be multiple
getWellFeatures = function(x, uname, feature=TRUE) {
  fd = fData(x)
  
  fduname1 = prw2uname(fd$plate, 1, well=fd$well)
  
  prw = uname2prw(uname)
  uname1 = prw2uname(prw$plate, 1, prw$row, prw$col)
  
  fd = fd[match(uname1, fduname1), feature]
  if (is.matrix(fd) || is.data.frame(fd)) rownames(fd) = uname
  else names(fd) = uname
  fd
}

getImageConf = function(x) {
  x@imageConf
}

resync = function() {
  library('imageHTS')
  source('R/imageHTS.R')
  source('R/configure.R')
  source('R/segmentation.R')
  source('R/segmentation-methods.R')
  source('R/extractFeatures.R')
  source('R/extractFeatures-methods.R')
  source('R/summarize.R')
  source('R/cellPicker.R')
  source('R/classification.R')
  source('R/webQuery.R')
  source('R/quality.R')
}

msg = function(...) {
  if (options()$verbose) cat(...,"\n")
  else cat(substr(...,1,1))
}
