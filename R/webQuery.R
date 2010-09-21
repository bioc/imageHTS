installWebQuery = function(x) {
  if (is.null(geneAnno(x))) stop("'x' must be annotated before using 'installWebQuery'")

  ## make and copy the webquery directory
  src = dir(system.file('webquery', package='imageHTS'), full.names=TRUE)
  dest = file.path(x@localPath, 'webquery')
  dir.create(dest, showWarnings=FALSE)
  for (s in src) file.copy(s, dest, overwrite=TRUE)

  ## make the annotation-webquery.txt file
  rc = well2rowcol(fData(x)[, 'well'])
  uname1 = prw2uname(plate=fData(x)[,'plate'], replicate=1, row=rc$row, col=rc$col)
  uname2 = prw2uname(plate=fData(x)[,'plate'], replicate=2, row=rc$row, col=rc$col)
  controlStatus = as.character(fData(x)[, 'controlStatus'])
  geneID = geneAnno(x)
  df = cbind(uname1=uname1, controlStatus=toupper(controlStatus), geneID=toupper(geneID), uname2=uname2)
  fann = fileHTS(x, type='file', filename='webquery/annotation-webquery.txt', createPath=TRUE, access='local')
  write.table(df, file=fann, row.names=FALSE, col.names=FALSE, quote=FALSE, sep='\t')

  ## make the conf.php file, describing the screen
  nbPlates = max(plate(x))
  ic = getImageConf(x)
  nbReplicates = length(ic$ReplicateNames)
  nbRows = pdim(x)[1]
  nbCols = pdim(x)[2]
  conf = '<?php\n';
  conf = paste(conf, '$nbplates = ', nbPlates, ';\n', sep='')
  conf = paste(conf, '$nbreplicates = ', nbReplicates, ';\n', sep='')
  conf = paste(conf, '$nbrows = ', nbRows, ';\n', sep='')
  conf = paste(conf, '$nbcols = ', nbCols, ';\n', sep='')
  conf = paste(conf, '$montagex = ', ic$Montage[1], ';\n', sep='')
  conf = paste(conf, '$montagey = ', ic$Montage[2], ';\n', sep='')
  conf = paste(conf, '$assayname = "', ic$AssayName, '";\n', sep='')
  conf = paste(conf, '?>')
  fconf = fileHTS(x, type='file', filename='webquery/conf.php', createPath=TRUE, access='local')
  write(conf, file=fconf)
}

writeThumbnail = function(x, uname, input.image, p, output.filename, access='cache') {
  if (missing(input.image)) input.image = try(readImage(fileHTS(x, 'viewfull', uname=uname, access=access)), silent=TRUE)
  if (missing(output.filename)) output.filename = fileHTS(x, 'viewthumb', uname=uname, createPath=TRUE, access='local')
  
  if (class(input.image)=='try-error' || length(input.image)==0) return(FALSE)
  else {
    if (is.null(p$thumbnail.crop)) stop('\'thumbnail.crop\' parameter is missing')
    if (is.null(p$thumbnail.resize.width)) stop('\'thumbnail.resize.width\' parameter is missing')
  
    p$thumbnail.crop = as.numeric(p$thumbnail.crop)
    p$thumbnail.resize.width = as.numeric(p$thumbnail.resize.width)

    thumb = input.image[p$thumbnail.crop[1]:p$thumbnail.crop[2],
      p$thumbnail.crop[3]:p$thumbnail.crop[4],]
    thumb = resize(thumb, w=p$thumbnail.resize.width)
    
    writeImage(thumb, output.filename, quality=95)
    return(TRUE)
  }
}

popWebQuery = function(x, access='server', browse=TRUE) {
  url = fileHTS(x, type='file', filename='webquery', access=access)
  if (access!='server') url = paste('file://', url, sep='')
  if (browse) {
    cat("Loading a Web browser on", url, "\n") 
    browseURL(url)
    invisible(url)
  } else return(url)
}
