## Actin, Tubulin, Hoetsch (DNA)
segmentATH = function(x, uname, p, access) {
  montage = getImageConf(x)$Montage

  ## read files
  fA = fileHTS(x, 'source', uname=uname, channel=1, access=access)
  fT = fileHTS(x, 'source', uname=uname, channel=2, access=access)
  fH = fileHTS(x, 'source', uname=uname, channel=3, access=access)
  fe = file.exists(c(fA, fT, fH))
  if (any(!fe)) stop('Missing source files: cannot open ', paste(c(fA, fT, fH)[!fe], collapse=' & '))

  ## evaluate R expressions for filters
  pev = c('nuc.athresh.filter', 'nuc.morpho.kernel', 'adj.nuc.kernel', 'cell.thresh.filter', 'cell.morpho.kernel')
  p[pev] = lapply(p[pev], function(z) eval(parse(text=paste(z, collapse=','))))

  ## all numeric except pev and some
  pnum = c(pev, 'seg.method', 'adj.a', 'adj.t', 'adj.h')
  pnum = !names(p)%in%pnum
  p[pnum] = lapply(p[pnum], as.numeric)

  msg("read images")
  read.scale = p[['read.scale']]
  if (is.null(read.scale)) read.scale = 1
  a = readImage(fA)*read.scale
  t = readImage(fT)*read.scale
  h = readImage(fH)*read.scale
  fe = c(length(a), length(t) ,length(h))!=0
  if (any(!fe)) stop('Missing source files: cannot open ', paste(c(fA, fT, fH)[!fe], collapse=' & '))
  
  msg("computing nmask")
  nmask = h > filter2(h, p$nuc.athresh.filter) + p$nuc.athresh.t
  nmask = opening(closing(nmask, p$nuc.morpho.kernel), p$nuc.morpho.kernel)
  nmask = fillHull(nmask)

  ## auto adjust illumination is the parameter 'adj.nuc.kernel' is present
  ## it is better to avoid this step
  if (!is.null(p[['adj.nuc.kernel']])) {
    msg("adjusting ath")
    smask = dilate(nmask, p$adj.nuc.kernel) - erode(nmask, p$adj.nuc.kernel)
    a = a - mean(a[a<=quantile(a, p$adj.quant)])
    a[a<0] = 0.0
    a = a/mean(a[smask>0])
    t = t - mean(t[t<=quantile(t, p$adj.quant)])
    t[t<0] = 0.0
    t = t/mean(t[smask>0])
    h = h - mean(h[h<=quantile(h, p$adj.quant)])
    h[h<0] = 0.0
    h = h/mean(h[nmask>0])
  }

  ## manual enhancement of images
  a = eval(parse(text=p$adj.a))
  t = eval(parse(text=p$adj.t))
  h = eval(parse(text=p$adj.h))
  
  msg("computing mix")
  mix = (a^2+t^2+h^2)^0.5
  cmask = filter2(mix,p$cell.thresh.filter)>=p$cell.thresh.t
  cmask = closing(cmask, p$cell.morpho.kernel)
  nmask[!(cmask>0)] = 0.0
  
  msg("untile images")
  a = untile(a, montage)
  t = untile(t, montage)
  h = untile(h, montage)
  mix = untile(mix, montage)
  nmask = untile(nmask, montage)
  cmask = untile(cmask, montage)
  
  msg("segmenting nuclei using watershed")
  dimh = dim(h)
  nmask = watershed(distmap(nmask), p$nuc.watershed.tolerance, p$nuc.watershed.neighbourood)
  nfts = lapply(1:dimh[3], function(i) {
    nmask0 <- getFrame(nmask, i)
    h0 <- getFrame(h, i)
    cbind(computeFeatures.basic(nmask0, h0), computeFeatures.shape(nmask0))
  })
  rmindex = lapply(nfts,
    function(n) which(n[,"b.mean"]<p$nuc.min.density |
                      n[,"s.area"]<p$nuc.min.size |
                      n[,"s.area"]>p$nuc.max.size))
  nmask = rmObjects(nmask, rmindex)
  nseg = fillHull(nmask)
  
  msg("segmenting cells using Voronoi tesselation")
  cmask = fillHull(propagate(mix^p$cell.propagate.mix.power, nseg, cmask, p$cell.propagate.lambda))
  storage.mode(cmask) <- "integer"
  cfts = lapply(1:dimh[3], function(i) {
    cmask0 <- getFrame(cmask, i)
    mix0 <- getFrame(mix, i)
    cbind(computeFeatures.basic(cmask0, mix0), computeFeatures.shape(cmask0))
  })
  
  msg("filtering bad cells")
  rmindex = lapply(cfts, function(c) 
    which(c[,"b.mean"]<p$cell.min.density | 
          c[,"s.area"]<p$cell.min.size |
          c[,"s.area"]>p$cell.max.size |
          c[,"s.perimeter"]>p$cell.max.perimeter))
  cmask = rmObjects(cmask, rmindex)
  nseg = rmObjects(nseg, rmindex)
  cseg = fillHull(cmask)
  nseg = fillHull(nseg)

  res = list(cal=rgbImage(red=a, green=t, blue=h), nseg=nseg, cseg=cseg)
  
  msg("segmentATH OK")
  cat(' nbcells=', countObjects(cseg), sep='')
  res
}
