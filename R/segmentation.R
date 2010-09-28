segmentWells = function (x, uname, segmentationPar, access='cache', writeData=TRUE) {
  ## several unames ?
  if (length(uname)>1) {
    for (u in uname) {
      z = try(segmentWells(x, uname=u, segmentationPar=segmentationPar, access=access), silent=TRUE)
      if (class(z)=='try-error') cat(z, ' KO\n')
    }
    return(invisible())
  }

  ## read parameters
  cat(uname, ': ',sep='')
  p = readHTS(x, type='file', filename=segmentationPar, access=access, format='dcf')
  montage = getImageConf(x)$Montage

  ## call segmentation method
  segMethod = p$seg.method
  z = eval(call(segMethod, x=x, uname=uname, p=p, access=access))

  ## write output segmentation results to disk 
  cal = z$cal
  nseg = z$nseg
  cseg = z$cseg

  if (writeData) {
    ## saving calibrated images
    ff = fileHTS(x, 'cal', uname=uname, createPath=TRUE, access='local')
    save(cal, file=ff, compress=TRUE)
    
    ## saving segmentation data
    ff = fileHTS(x, 'seg', uname=uname, createPath=TRUE, access='local')
    seg = list(nseg=nseg, cseg=cseg)
    save(seg, file=ff, compress=TRUE)
    
    ## write calibrated, unmonted images
    nbimages = getNumberOfFrames(cal, 'render')
    for (spot in 1:nbimages) {
      if (length(dim(cal))==3) caspot = cal
      else caspot = cal[,,,spot]
      ff = fileHTS(x, 'viewunmonted', uname=uname, spot=spot, createPath=TRUE, access='local')
      writeImage(caspot, file=ff, quality=95)
    }
    
    ## write calibrated, monted images
    ff = fileHTS(x, 'viewfull', uname=uname, createPath=TRUE, access='local')
    viewfull = tile(cal, montage)
    writeImage(viewfull, ff, quality=95)
    
    ## write calibrated images with segmentation information
    ff = fileHTS(x, 'viewseg', uname=uname, createPath=TRUE, access='local')
    writeImage(tile(highlightSegmentation(cal, nseg, cseg), montage), ff, quality=95)
    
    ## write calibrated, thumbnail images for webQuery
    writeThumbnail(x, uname=uname, p=p, input.image=viewfull)
    
    ## write contours for cellPicker
    writeContours(x, uname, access=access)
  }
    
  cat(' OK\n')
  invisible(z)
}

countObjects = function(cseg) {
  if (length(dim(cseg))==3) sum(apply(cseg, 3, max))
  else max(cseg)
}

paintObjectsOpaque = function(seg, cal, ...) {
  bcal = Image(0, dim=dim(cal), colormode=colorMode(cal))
  bcal = paintObjects(seg, bcal, col='white')
  cal[as.logical(bcal)] = 0
  paintObjects(seg, cal, ...)
}

highlightSegmentation = function(cal, nseg=NULL, cseg=NULL, thick=FALSE) {
  if (thick) {
    if (!is.null(nseg)) {
      if (colorMode(cal)==Grayscale) cal = EBImage::channel(cal, 'rgb')
      w = nseg!=translate(nseg, c(0,1)) | nseg!=translate(nseg, c(1,0)) | nseg!=translate(nseg, c(0,-1)) | nseg!=translate(nseg, c(-1,0))
      w = w==1
      cal[rep(w, 3)] = c(rep(1, sum(w)*2), rep(0, sum(w)))
    }

    if (!is.null(cseg)) {
      w = cseg!=translate(cseg, c(0,1)) | cseg!=translate(cseg, c(1,0)) | cseg!=translate(cseg, c(0,-1)) | cseg!=translate(cseg, c(-1,0))
      w = w==1
      cal[rep(w, 3)] = c(rep(1, sum(w)), rep(0, sum(w)), rep(1, sum(w)))
    }
    cal
  } else {
    if (!is.null(nseg)) hseg = paintObjectsOpaque(nseg, cal, col=c('#ffff00'))
    else hseg = cal
    if (!is.null(cseg)) hseg = paintObjectsOpaque(cseg, hseg, col=c('#ff00ff'))
    hseg
  }
}
