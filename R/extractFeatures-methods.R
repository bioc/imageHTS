getCellFtrsATH = function(cal, seg) {
  nf = getNumberOfFrames(cal, 'render')
  nseg = seg$nseg
  cseg = seg$cseg

  ftrs = lapply(1:nf, function(i) {
    if (length(dim(cseg))==3) {
      c = cseg[,,i]
      n = nseg[,,i]
    } else {
      c = cseg
      n = nseg
    }

    ## image with no cells
    if (countObjects(c)==0) return(NA)
    if (length(dim(cal))==4) {
      a = cal[,,1,i]
      t = cal[,,2,i]
      h = cal[,,3,i]
    } else {
      a = cal[,,1]
      t = cal[,,2]
      h = cal[,,3]
    }
    m = sqrt(a^2+t^2)

    ## geometric features (gc = geometry of the cell, gn = geometry of the nucleus)
    msg('geom')
    gc = computeFeatures.shape(c)
    gn = computeFeatures.shape(n)
    colnames(gc) = paste('c.', colnames(gc), sep='')
    colnames(gn) = paste('n.', colnames(gn), sep='')
    
    ## moment features (mca = moments computed on the channel a over the cell)
    msg('moments')
    mca = computeFeatures.moment(c, a) 
    mct = computeFeatures.moment(c, t)
    mch = computeFeatures.moment(c, h)
    mcm = computeFeatures.moment(c, m)
    mna = computeFeatures.moment(n, a)
    mnt = computeFeatures.moment(n, t)
    mnh = computeFeatures.moment(n, h)
    colnames(mca) = paste('c.a.', colnames(mca), sep='')
    colnames(mct) = paste('c.t.', colnames(mct), sep='')
    colnames(mch) = paste('c.h.', colnames(mch), sep='')
    colnames(mcm) = paste('c.m.', colnames(mcm), sep='')
    colnames(mna) = paste('n.a.', colnames(mna), sep='')
    colnames(mnt) = paste('n.t.', colnames(mnt), sep='')
    colnames(mnh) = paste('n.h.', colnames(mnh), sep='')

    ## basic features
    msg('basic')
    bca = computeFeatures.basic(c, a) 
    bct = computeFeatures.basic(c, t) 
    bch = computeFeatures.basic(c, h) 
    bcm = computeFeatures.basic(c, m) 
    bna = computeFeatures.basic(n, a) 
    bnt = computeFeatures.basic(n, t) 
    bnh = computeFeatures.basic(n, h) 
    colnames(bca) = paste('c.a.', colnames(bca), sep='')
    colnames(bct) = paste('c.t.', colnames(bct), sep='')
    colnames(bch) = paste('c.h.', colnames(bch), sep='')
    colnames(bcm) = paste('c.m.', colnames(bcm), sep='')
    colnames(bna) = paste('n.a.', colnames(bna), sep='')
    colnames(bnt) = paste('n.t.', colnames(bnt), sep='')
    colnames(bnh) = paste('n.h.', colnames(bnh), sep='')

    ## haralick features
    msg('haralick')
    hca = computeFeatures.haralick(c, a)
    hct = computeFeatures.haralick(c, t)
    hch = computeFeatures.haralick(c, h)
    hcm = computeFeatures.haralick(c, m)
    hna = computeFeatures.haralick(n, a)
    hnt = computeFeatures.haralick(n, t)
    hnh = computeFeatures.haralick(n, h)
    colnames(hca) = paste('c.a.', colnames(hca), sep='')
    colnames(hct) = paste('c.t.', colnames(hct), sep='')
    colnames(hch) = paste('c.h.', colnames(hch), sep='')
    colnames(hcm) = paste('c.m.', colnames(hcm), sep='')
    colnames(hna) = paste('n.a.', colnames(hna), sep='')
    colnames(hnt) = paste('n.t.', colnames(hnt), sep='')
    colnames(hnh) = paste('n.h.', colnames(hnh), sep='')
    
    ## Correlated features (catc = correlation between a and t over the cell)
    msg('correlations')
    a=as.numeric(a)
    t=as.numeric(t)
    h=as.numeric(h)
    c=as.numeric(c)
    n=as.numeric(n)
    sac = split(a, c)[-1]
    stc = split(t, c)[-1]
    shc = split(h, c)[-1]
    san = split(a, n)[-1]
    stn = split(t, n)[-1]
    shn = split(h, n)[-1]
    catc = mapply(stats::cor, sac, stc)
    cahc = mapply(stats::cor, sac, shc)
    cthc = mapply(stats::cor, stc, shc)
    catn = mapply(stats::cor, san, stn)
    cahn = mapply(stats::cor, san, shn)
    cthn = mapply(stats::cor, stn, shn)
    cor = cbind(c.at.cor=catc, c.ah.cor=cahc, c.th.cor=cthc,
      n.at.cor=catn, n.ah.cor=cahn, n.th.cor=cthn)

    cbind(gc, gn,
          mca, mct, mch, mcm, mna, mnt, mnh,
          bca, bct, bch, bcm, bna, bnt, bnh,
          hca, hct, hch, hcm, hna, hnt, hnh,
          cor)
  })
  
  ## merge lists in a data.frame that contains uname, spot, id
  ftrs = do.call(rbind, mapply(function(f, i) data.frame(spot=i, id=1:nrow(f), f, stringsAsFactors=FALSE), ftrs, 1:length(ftrs), SIMPLIFY=FALSE))

  ## NAs to zero
  ftrs[is.na(ftrs)] = 0
  
  ftrs
}
