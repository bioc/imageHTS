extractFeatures = function(x, uname, featurePar, access='cache') {
  ## read parameters
  p = readHTS(x, type='file', filename=featurePar, access=access, format='dcf')
  efMethod = p$extractfeatures.method
  
  for (u in uname) {
    cat(u,': ', sep='')
    cal = try(readHTS(x, 'cal', uname=u, access=access))
    seg  = try(readHTS(x, 'seg', uname=u, access=access))  
    if (class(cal)!='try-error' & class(seg)!='try-error') {
      ftrs = eval(call(efMethod, cal, seg))

      ## default writing data scheme
      write.seg.tab = TRUE
      
      ## update writing data scheme
      if (!is.null(p$feature.write.seg.tab)) write.seg.tab = as.logical(p$feature.write.seg.tab)

      if (write.seg.tab) {
        fftrs = fileHTS(x, 'ftrs', uname=u, createPath=TRUE, access='local')
        write.table(ftrs, file=fftrs, sep='\t', quote=FALSE, row.names=FALSE, col.names=TRUE)
      }
      
      cat(' OK\n')
    } else cat('NA\n')
  } 
}

## compute geometric features on mask x 
## - ec captures the eccentricity (sqrt of ecc, better than ecc)
## - ss captures the smooth area (product of moment axes)
## - renames h. into g. (geometric)
geofeatures = function(x) {
  g = hullFeatures(x)
  gf = c('g.x', 'g.y', 'g.s', 'g.p', 'g.pdm', 'g.I1', 'g.I2')
  l1 = g[,'g.l1']
  l2 = g[,'g.l2']
  g = cbind(g[,gf,drop=FALSE], g.ec=1-l2/l1, g.ss=4*pi*sqrt(l1*l2))
  g 
}

## compute moments of mask x using values y
## - ec captures the eccentricity (sqrt of ecc, better than ecc)
## - ss captures the smooth area (product of moment axes)
## - adds name prefix m. (moments)
mymoments = function(x, y) {
  m = moments(x, y)
  mf = c('m.int', 'm.I1', 'm.I2')
  l1 = m[,'m.l1']
  l2 = m[,'m.l2']
  m = cbind(m[,mf,drop=FALSE], m.ec=1-l2/l1, m.ss=4*pi*sqrt(l1*l2))
  m
}

## compute Haralick features of mak x using values y
## - renames t. into h. (Haralick))
myharalickFeatures = function(x, y) {
  hf = 1:12
  h = suppressWarnings(haralickFeatures(x, y)[,hf,drop=FALSE])
  h
} 
