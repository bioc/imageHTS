writeContours = function(x, uname, tolErr=3, access) {
  seg = readHTS(x, 'seg', uname=uname, access=access)
  cseg = seg[['cseg']]
  if (length(dim(cseg))==2) n = 1
  else n = dim(cseg)[3]
  
  for (i in 1:n) {
    if (length(dim(cseg))==2) cs = cseg
    else cs = cseg[,,i]
    fc = fileHTS(x, 'contour', uname=uname, spot=i, createPath=TRUE, access='local')		
    con = file(fc, "wt")			
    cellNumber = countObjects(cs)
    write(paste("cellNumber=", cellNumber, ";\n", sep=""),con)
    
    ## writing the coords of the vertices of the polygons
    coords=getCoords(cs, tolErr=tolErr)
    write("contours=new Array();",con)		
    for (cellId in seq_len(cellNumber)) write(paste("contours[",cellId,"] = new Array(",paste(coords[[cellId]][1:length(coords[[cellId]])], sep = "", collapse=","),");",sep=""),con)
    close(con)
  }
}

## install cellPicker files in the project path
installCellPicker = function(x) {
  src = system.file('cellpicker', 'cellpicker.html', package='imageHTS')
  dest = x@localPath
  file.copy(src, dest, overwrite=TRUE)

  src = dir(system.file('cellpicker', 'css', package='imageHTS'), full.names=TRUE)
  dest = file.path(x@localPath, 'css')
  dir.create(dest, showWarnings=FALSE)
  for (s in src) file.copy(s, dest, overwrite=TRUE)
  
  src = dir(system.file('cellpicker', 'javascript', package='imageHTS'), full.names=TRUE)
  dest = file.path(x@localPath, 'javascript')
  dir.create(dest, showWarnings=FALSE)
  for (s in src) file.copy(s, dest, overwrite=TRUE)
  
  invisible(NULL)
}

popCellPicker = function(x, uname, spot, id.highlight, access='server', browse=TRUE) {
  if (x@serverURL=='') access='local'
  plabel = 'X'

  if (missing(spot)) spot = rep(1, length(uname))
  else if (length(spot)!=length(uname)) stop("'uname' and 'spot' must have the same length")
  unamespot = paste(uname, spot, sep="-")

  if (missing(id.highlight)) {
    unamespotid = unique(unamespot)
  } else {
    if (length(id.highlight)!=length(uname)) stop("'uname' and 'id.highlight' must have the same length")
    unamespotid = tapply(id.highlight, unamespot, paste, collapse="@")
    unamespotid = paste(names(unamespotid), unamespotid, sep="@")
  }
  
  images = paste(unamespotid, collapse="+")
  
  url = fileHTS(x, type='file', filename='cellpicker.html', access=access)
  if (access!='server') url = paste('file://', url, sep='')
  url = paste(url, '?plabel=', plabel, '&display=', images, sep='')
  if (browse) {
    cat("Loading a Web browser on", url, "\n") 
    browseURL(url)
    invisible(url)
  } else return(url)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## write a .js file containing the cell labels
## labels : a vector of characters of the same length as the number of cells in the image
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
writeLabels = function(cpProject, imageID, labels=NULL, overwrite=FALSE) {
  labelsfilename = file.path(cpProject,'labels',paste('labels_',imageID,'.js',sep=''))		
  if(file.exists(labelsfilename) & overwrite==FALSE) {
    print(paste(labelsfilename,'already exists. Use overwrite=TRUE to overwrite it.', sep=' '))
  } else {	
    con = file(labelsfilename, "wt")		
    write('cellLabels = new Array();',con)	
    if(!is.null(labels)) {
      write(paste('cellLabels[',1:length(labels),'] = "',labels,'"',sep=''),con,append=TRUE)
    }
    close(con)	
  }
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Takes a matrix such as the output of 'getOrientedContour' as input, 
## and re-order the rows so that row[index] becomes the first row. 
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sortOrientedContour = function(orientedContour) {
  if(is.na(sum(orientedContour))) {return(orientedContour)}
  
  ## first we get the center of the set of points	
  rowc = round(mean(orientedContour[,1]))
  colc = round(mean(orientedContour[,2]))	
  
  ## then we find the index of the farthest point of the contour from the center 	
  seq=1:nrow(orientedContour)
  dist=(orientedContour[seq,1]-rowc)*(orientedContour[seq,1]-rowc)+(orientedContour[seq,2]-colc)*(orientedContour[seq,2]-colc)
  index=min(which(dist==max(dist)))
  if(index==1) {return(orientedContour)}
  else {return(rbind(orientedContour[index:nrow(orientedContour),],orientedContour[1:(index-1),]))}
}

getError = function(startPoint, endPoint, testedPoints, sortedOrientedContour) {		
  errs=rep(NA, length(testedPoints))
  
  ## cos(theta(endPoint->startPoint, endPoint->testedPoint)) < 0
  u = sortedOrientedContour[startPoint,]-sortedOrientedContour[endPoint,]
  v = cbind(sortedOrientedContour[testedPoints,1]-sortedOrientedContour[endPoint,1], sortedOrientedContour[testedPoints,2]-sortedOrientedContour[endPoint,2])
  errs[v[,1]==0 & v[,2]==0]=0
  nu = sqrt(u[1]*u[1]+u[2]*u[2])
  nv = sqrt(v[,1]^2+v[,2]^2)
  errs[nu==0 | nv==0]=0
  cosT = (u[1]*v[,1]+u[2]*v[,2])/(nu*nv)
  z = is.na(errs)
  errs[cosT[z]<0]=nv[cosT[z]<0]
  
  ## cos(theta(startPoint->endPoint, startPoint->testedPoint)) < 0 
  u = -u
  v = cbind(sortedOrientedContour[testedPoints,1]-sortedOrientedContour[startPoint,1], sortedOrientedContour[testedPoints,2]-sortedOrientedContour[startPoint,2])
  errs[v[,1]==0 & v[,2]==0]=0
  nv = sqrt(v[,1]^2+v[,2]^2)
  errs[nv==0]=0
  cosT = (u[1]*v[,1]+u[2]*v[,2])/(nu*nv)
  z = is.na(errs)
  errs[cosT[z]<0]=nv[cosT[z]<0]		
  
  ## else (same theta as in the previous case)
  sinT = sqrt(abs(1-cosT*cosT))  ## using cos²x + sin²x = 1. 'abs' is here for the computional pb of 1-1 = -1.10^(-16)	
  z = is.na(errs)
  errs[z] = nv[z]*sinT[z]
  return(errs)
}

getCoords = function(segmentedImage, tolErr=3) {
  ## initialization	
  cellNumber = countObjects(segmentedImage)
  finalCoords = list()

  orientedContours=ocontour(segmentedImage)
  
  ## processing for each cell
  cellId=1
  while(cellId < (cellNumber+1)) {
    try({
      ## cat('cellId',cellId,'\n')
      ## getting the oriented contour starting by the pixel the farthest from the center of the object
      orientedContour=orientedContours[[cellId]]					
      sortedOrientedContour=sortOrientedContour(orientedContour)	
      
      ## starting the extraction of vertices
      if(is.na(sum(sortedOrientedContour))) {finalCoords[[cellId]]=NULL}
      else {
        ## extractiong polygons from the sorted oriented contour
        newCoord= c(sortedOrientedContour[1,1],sortedOrientedContour[1,2])			
        startPoint=1	
        endPoint=3			
        ## while there are more vertices to extract
        while(endPoint<nrow(sortedOrientedContour)) {
          endPoint=startPoint+2						
          maxError=0
          newVertex=startPoint+1			
          while(maxError<=tolErr & endPoint<nrow(sortedOrientedContour)) {            
            testedPoints=(startPoint+1):(endPoint-1)
            errs=getError(startPoint, endPoint, testedPoints, sortedOrientedContour)
            newVertex=testedPoints[which.max(errs)]
            maxError=max(errs)
            endPoint=endPoint+1		
          }	
          if(endPoint<nrow(sortedOrientedContour)) {
            newCoord= c(newCoord,sortedOrientedContour[newVertex,1],sortedOrientedContour[newVertex,2])					
          }
          startPoint=newVertex+1										
        }						
        finalCoords[[cellId]]=newCoord	
      }
      cellId=cellId+1		
    }, silent=TRUE)
  }		
  return (finalCoords)	
}
