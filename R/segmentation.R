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
  cal = z$cal
  nseg = z$nseg
  cseg = z$cseg
  
  ## default writing data scheme
  write.cal = TRUE
  write.cal.view = TRUE
  write.cal.view.tiled = TRUE
  write.seg = TRUE
  write.seg.view = FALSE
  write.seg.view.tiled = TRUE
  write.thumbnail = TRUE
  write.contours = TRUE

  ## update writing data scheme
  if (!is.null(p$seg.write.cal)) write.cal = as.logical(p$seg.write.cal)
  if (!is.null(p$seg.write.cal.view)) write.cal.view = as.logical(p$seg.write.cal.view)
  if (!is.null(p$seg.write.cal.view.tiled)) write.cal.view.tiled= as.logical(p$seg.write.cal.view.tiled)
  if (!is.null(p$seg.write.seg)) write.seg = as.logical(p$seg.write.seg)
  if (!is.null(p$seg.write.seg.view)) write.seg.view = as.logical(p$seg.write.seg.view)
  if (!is.null(p$seg.write.seg.view.tiled)) write.seg.view.tiled = as.logical(p$seg.write.seg.view.tiled)
  if (!is.null(p$seg.write.thumbnail)) write.thumbnail = as.logical(p$seg.write.thumbnail)
  if (!is.null(p$seg.write.contours)) write.contours = as.logical(p$seg.write.contours)

  if (writeData) {
    ## save calibrated data
    if (write.cal) {
      ff = fileHTS(x, 'cal', uname=uname, createPath=TRUE, access='local')
      save(cal, file=ff, compress=TRUE)
    }

    ## write calibrated images
    if (write.cal.view) {
      nbimages = getNumberOfFrames(cal, 'render')
      for (spot in 1:nbimages) {       
        ff = fileHTS(x, 'viewunmonted', uname=uname, spot=spot, createPath=TRUE, access='local')
        frame = getFrame(cal, spot, type='render')
        writeImage(frame, ff, quality=95)
      }
    }
    
    ## write calibrated images, tiled
    if (write.cal.view.tiled) {
      ff = fileHTS(x, 'viewfull', uname=uname, createPath=TRUE, access='local')
      viewfull = tile(cal, montage, fg.col='black', bg.col='black')
      writeImage(viewfull, ff, quality=95)
    }
    
    ## save segmentation data
    if (write.seg) {
      ff = fileHTS(x, 'seg', uname=uname, createPath=TRUE, access='local')
      seg = list(nseg=nseg, cseg=cseg)
      save(seg, file=ff, compress=TRUE)
    }
    
    ## prepare hseg
    if (write.seg.view || write.seg.view.tiled) hseg = highlightSegmentation(cal, nseg, cseg)
    
    ## write calibrated images with segmentation information
    if (write.seg.view) {
      nbimages = getNumberOfFrames(hseg, 'render')
      for (spot in 1:nbimages) {
        frame = getFrame(hseg, spot, type='render')
        ff = fileHTS(x, 'viewunmonted', uname=uname, spot=spot, createPath=TRUE, access='local')
        ff = gsub('_um.jpeg', '_us.jpeg', ff)
        writeImage(frame, ff, quality=95)
      }
    }
    
    ## write calibrated images with segmentation information, tiled
    if (write.seg.view.tiled) {
      ff = fileHTS(x, 'viewseg', uname=uname, createPath=TRUE, access='local')
      writeImage(tile(hseg, montage, fg.col='black', bg.col='black'), ff, quality=95)
    }
    
    ## write calibrated, thumbnail images for webQuery
    if (write.thumbnail) {
      frame = getFrame(cal, 1, type='render')
      writeThumbnail(x, uname=uname, p=p, input.image=frame)
    }
    
    ## write contours for cellPicker
    if (write.contours) {
      writeContours(x, uname, access=access)
    }
  }
    
  cat(' OK\n')
  invisible(z)
}

countObjects = function(cseg) {
  if (length(dim(cseg))==3) sum(apply(cseg, 3, max))
  else max(cseg)
}

## AO: is this function needed at all?
paintObjectsOpaque = function(seg, cal, ...) {
  bcal = Image(0, dim=dim(cal), colormode=colorMode(cal))
  bcal = paintObjects(seg, bcal, col='white')
  cal[as.logical(bcal)] = 0
  paintObjects(seg, cal, ...)
}

highlightSegmentation = function(cal, nseg=NULL, cseg=NULL, thick=FALSE) {
  if (colorMode(cal)==Grayscale) cal = EBImage::channel(cal, 'rgb')
  if (thick) {
    if (!is.null(nseg)) {
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
