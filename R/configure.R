parseImageConf = function(filename, localPath='myscreen', serverURL, access='cache') {
  if (missing(serverURL) || is.null(serverURL)) serverURL = ''
  
  ## make localPath an absolute file path
  if (!file.exists(localPath)) dir.create(localPath, recursive=TRUE)
  localPath = tools::file_path_as_absolute(localPath)

  ## force serverURL to be an URL
  if (serverURL!='') {
    if (regexpr('file://', serverURL)!=1 && regexpr('http://', serverURL)!=1) {
      if (file.exists(serverURL)) {
        serverURL = tools::file_path_as_absolute(serverURL)
        serverURL = paste('file://', serverURL, sep='')
      }
      else stop('parseImageConf: cannot access to serverURL=', serverURL) 
    }
  }
  
  ## parse imagelist file
  x = new('imageHTS', localPath=localPath, serverURL=serverURL)
  ic = readHTS(x, type='file', filename=filename, access=access, format='dcf')

  ## check missing fields
  ifields = c('SourceFilenamePattern', 'PlateNames', 'ReplicateNames', 'RowNames', 'ColNames', 'ChannelNames')
  ifm = which(is.na(match(ifields,names(ic))))
  if (length(ifm)>0) {
    warning(paste('Assuming empty value for missing field(s):', ifields[ifm]))
    ic[ifm] = ''
  }
  
  nic = sapply(ic,length)
  nbPlates = nic['PlateNames']
  nbReplicates = nic['ReplicateNames']
  nbRows = nic['RowNames']
  nbCols = nic['ColNames']
  nbWells = nbRows*nbCols
  nbChannels = nic['ChannelNames']

  if (!is.null(ic$Montage)) ic$Montage = as.numeric(ic$Montage)
  else ic$Montage = c(1, 1)

  if (!is.null(ic$SpotNames)) nbSpots = length(ic$SpotNames)
  else nbSpots = 1
  
  ## report
  cat('File "',filename, '" read.\n', sep='')
  cat('Number of plates=', nbPlates,'\n')
  cat('Number of replicates=', nbReplicates,'\n')
  cat('Number of wells=', nbWells,'\n')
  cat('Number of channels=', nbChannels,'\n')
  cat('Number of spots=', nbSpots,'\n')
                                
  ## nbChannels (cellHTS2 channels, not image channels) is not known at this stage
  ## build an cellHTS object filled with NA, with only one channel
  aplates = rep(1:nbPlates, each=nbReplicates*nbWells)
  areplicates = rep(rep(1:nbReplicates, each=nbWells), nbPlates)
  awells =  rep(paste(rep(ic[['RowNames']][1:nbRows], each=nbCols), rep(rep(ic[['ColNames']][1:nbCols], nbRows)), sep=''), nbPlates*nbReplicates)
  channel1 = rep(NA, nbPlates*nbReplicates*nbWells)
  z = data.frame(Plate=aplates, Replicate=areplicates, Well=awells, channel1=channel1)

  xst = buildCellHTS(z, assayName=ic$AssayName, verbose=FALSE, returnSlots=TRUE)
  do.call(new, c(list("imageHTS"), xst, list(localPath=localPath, serverURL=serverURL, imageConf = ic)))
}

## Build a cellHTS2 object from a data.frame that must contain the columns (case dependant):
## - Plate
## - Replicate
## - Well
## - extra fields are interpreted as channel values
## If missing, nbPlates, nbReplicates, nbRows, nbCols are estimated by taking the maximum respective values in x
## assayNames specifies the name of the assay
## If returnStructures = TRUE, doesn't construct the cellHTS object but just returns the slots needed to build a cellHTS2 object
buildCellHTS = function(x, assayName, channelNames=NULL, nbPlates, nbReplicates, nbRows, nbCols, verbose=TRUE, returnSlots=FALSE) {
  ## separate x
  id = c('Plate', 'Replicate', 'Well')
  xid = x[id]
  for (i in id) x[i] = NULL
  x = as.matrix(x)

  if (missing(nbPlates)) nbPlates = max(xid$Plate)
  if (missing(nbReplicates)) nbReplicates = max(xid$Replicate)
  if (missing(nbRows)) {
    let = match(substr(xid$Well, 1, 1), LETTERS)
    num = as.integer(substr(xid$Well, 2, 3))
    if(any(is.na(let)) || any(is.na(num))) stop(sprintf("Malformated column 'Well'"))
    nbRows=max(let)
    nbCols=max(num)
  }

  dimPlate = c(nbRows, nbCols)
  names(dimPlate)=c('nrow', 'ncol')
  nbWells = prod(dimPlate) 

  unames = sprintf('%03d-%03d-%s', xid$Plate, xid$Replicate, xid$Well)
  nbChannels = ncol(x)
  if (is.null(channelNames)) channelNames = paste("Channel", seq_len(nbChannels))
 
  if (verbose) {
    cat('Number of plates=',nbPlates,'\n')
    cat('Number of replicates=',nbReplicates,'\n')
    cat('Number of wells= ',nbWells,' dimPlate= (',paste(dimPlate, collapse=','),')\n',sep='')
    cat('Number of channels=',nbChannels,'\n')
  }
  
  ## build assayData
  nbFeatures=nbPlates*nbWells
  xtemp=array(NA, dim=c(nbFeatures, nbReplicates, nbChannels))
  for (i in 1:nbReplicates) {
    z = xid$Replicate==i
    runames = sprintf('%03d-%03d-%s', xid$Plate[z], i, xid$Well[z])
    z = match(unames, runames)
    xtemp[na.omit(z), i, ] = x[!is.na(z),]
  }
  dat = lapply(1:nbChannels, function(ch) matrix(xtemp[,,ch,drop=FALSE], ncol=nbReplicates, dimnames=list(Features=1:nbFeatures, Sample=1:nbReplicates)))
  names(dat) = channelNames
  adata = do.call(assayDataNew, c(storage.mode="lockedEnvironment", dat))
    
  ## build phenoData
  pdata = new("AnnotatedDataFrame",
               data <- data.frame(replicate=seq_len(nbReplicates),
                                  assay=rep(assayName, nbReplicates),
                                  stringsAsFactors=FALSE),
               varMetadata=data.frame(labelDescription=c("Replicate number",
                                        "Biological assay"),
                 channel=factor(rep("_ALL_", 2L),
                   levels=c(ls(adata), "_ALL_")),
                 row.names=c("replicate", "assay"),
                 stringsAsFactors=FALSE))
  
  ## build featuresData
  wells = convertWellCoordinates(1:nbWells, pdim=dimPlate)$letnum
  fdata = new("AnnotatedDataFrame", 
    data = data.frame(plate=rep(seq_len(nbPlates), each=nbWells),
                       well=rep(wells, nbPlates), 
                       controlStatus=factor(rep("unknown", nbPlates*nbWells)),
                       stringsAsFactors=FALSE), 
    varMetadata=data.frame(labelDescription=c("Plate number", "Well ID",
                             "Well annotation"),
      row.names=c("plate", "well", "controlStatus"),
      stringsAsFactors=FALSE))

  ## build pd (plate description)(plate * replicate * channel)
  aplates = rep(1:nbPlates, each=nbReplicates*nbChannels)
  areplicates = rep(rep(1:nbReplicates, each=nbChannels), nbPlates)
  achannels = rep(1:nbChannels, nbPlates*nbReplicates)
  pd = data.frame(Filename=channelNames[achannels], Plate=aplates, Replicate=areplicates, Channel=achannels, errorMessage=as.logical(NA), stringsAsFactors=FALSE) 
  status = rep(I('OK'), nrow(pd))
  intensityFiles = as.list(rep('', nrow(pd)))
    
  ## build batch
  batch = as.data.frame(matrix(ncol=nbReplicates, nrow=nbPlates))
  colnames(batch) = sprintf("replicate%d", seq_len(nbReplicates))
  rownames(batch) = sprintf("plate%d", seq_len(nbPlates))

  xst = list(assayData=adata, phenoData=pdata, featureData=fdata, plateList=cbind(pd[,1L,drop=FALSE], status=I(status), pd[,-1L,drop=FALSE]),
    intensityFiles=intensityFiles, plateData=list(Batch=batch))

  if (returnSlots) return(xst)
  else return(do.call(new, c(list('cellHTS'), xst)))
}
