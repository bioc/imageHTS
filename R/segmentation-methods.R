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
 
  msg("adding edge of the cell mask to nuclei mask")
  dimh = dim(h)
  tmp = cmask
  tmp[2:(dimh[1]-1),2:(dimh[2]-1),] = 0.0
  nmask[tmp>0] = 1.0
  cmask[nmask>0] = 1.0

  msg("segmenting nuclei using watershed")
  nmask = watershed(distmap(nmask), p$nuc.watershed.tolerance, p$nuc.watershed.neighbourood)
  hf = hullFeatures(nmask); if (dimh[3]==1) hf = list(hf)
  mom = cmoments(nmask, h); if (dimh[3]==1) mom = list(mom)
  nfts = mapply(function(x,y) as.data.frame(cbind(x,y)), hf, mom, SIMPLIFY=FALSE)
  rmindex = lapply(nfts,
    function(n) which(n$m.int/n$m.pxs<p$nuc.min.density |
                      n$m.pxs<p$nuc.min.size |
                      n$m.pxs>p$nuc.max.size))
  nseg = fillHull(rmObjects(nmask, rmindex))
  
  msg("segmenting cells using Voronoi tesselation")
  cmask = fillHull(propagate(mix^p$cell.propagate.mix.power, nseg, cmask, p$cell.propagate.lambda))
  hf = hullFeatures(cmask);   if (dimh[3]==1) hf = list(hf)
  mom = cmoments(cmask, mix); if (dimh[3]==1) mom = list(mom)
  cfts = mapply(function(x,y) as.data.frame(cbind(x,y)), hf, mom, SIMPLIFY=FALSE)

  msg("filtering bad cells")
  rmindex = lapply(cfts, function(c) 
    which(c$g.edge/c$g.p>p$cell.max.edgepratio | c$m.int/c$m.pxs<p$cell.min.density | 
          c$m.pxs<p$cell.min.size | c$m.pxs>p$cell.max.size | c$g.p>p$cell.max.perimeter))
  cseg = fillHull(rmObjects(cmask, rmindex))
  nseg = fillHull(rmObjects(nseg, rmindex))

  res = list(cal=rgbImage(r=a, g=t, b=h), nseg=nseg, cseg=cseg)
  
  msg("segmentATH OK")
  cat(' nbcells=', countObjects(cseg), sep='')
  res
}
