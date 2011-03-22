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
    gc = geofeatures(c)
    gn = geofeatures(n)
    colnames(gc) = paste('c.', colnames(gc), sep='')
    colnames(gn) = paste('n.', colnames(gn), sep='')
    
    ## moment features (mca = moments computed on the channel a over the cell)
    msg('moments')
    mca = mymoments(c, a) 
    mct = mymoments(c, t)
    mch = mymoments(c, h)
    mcm = mymoments(c, m)
    mna = mymoments(n, a)
    mnt = mymoments(n, t)
    mnh = mymoments(n, h)
    colnames(mca) = paste('c.a.', colnames(mca), sep='')
    colnames(mct) = paste('c.t.', colnames(mct), sep='')
    colnames(mch) = paste('c.h.', colnames(mch), sep='')
    colnames(mcm) = paste('c.m.', colnames(mcm), sep='')
    colnames(mna) = paste('n.a.', colnames(mna), sep='')
    colnames(mnt) = paste('n.t.', colnames(mnt), sep='')
    colnames(mnh) = paste('n.h.', colnames(mnh), sep='')
   
    ## Zernike moments features (znh = Zernike moments computed on the channel h over the nucleus)
    msg('zernike')
    zf = c('z.0101', 'z.0202', 'z.0301', 'z.0303', 'z.0404')
    zca = zernikeMoments(c, a, N=4, R=60)[,zf,drop=FALSE]
    zct = zernikeMoments(c, t, N=4, R=60)[,zf,drop=FALSE]
    zch = zernikeMoments(c, h, N=4, R=60)[,zf,drop=FALSE]
    zcm = zernikeMoments(c, m, N=4, R=60)[,zf,drop=FALSE]
    zna = zernikeMoments(n, a, N=4, R=60)[,zf,drop=FALSE]
    znt = zernikeMoments(n, t, N=4, R=60)[,zf,drop=FALSE]
    znh = zernikeMoments(n, h, N=4, R=60)[,zf,drop=FALSE]
    colnames(zca) = paste('c.a.', colnames(zca), sep='')
    colnames(zct) = paste('c.t.', colnames(zct), sep='')
    colnames(zch) = paste('c.h.', colnames(zch), sep='')
    colnames(zcm) = paste('c.m.', colnames(zcm), sep='')
    colnames(zna) = paste('n.a.', colnames(zna), sep='')
    colnames(znt) = paste('n.t.', colnames(znt), sep='')
    colnames(znh) = paste('n.h.', colnames(znh), sep='')
    
    ## Haralick features
    msg('haralick')
    hca = myharalickFeatures(c, a)
    hct = myharalickFeatures(c, t)
    hch = myharalickFeatures(c, h)
    hcm = myharalickFeatures(c, m)
    hna = myharalickFeatures(n, a)
    hnt = myharalickFeatures(n, t)
    hnh = myharalickFeatures(n, h)
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
    catc = mapply(cor, sac, stc)
    cahc = mapply(cor, sac, shc)
    cthc = mapply(cor, stc, shc)
    catn = mapply(cor, san, stn)
    cahn = mapply(cor, san, shn)
    cthn = mapply(cor, stn, shn)
    cor = cbind(c.at.cor=catc, c.ah.cor=cahc, c.th.cor=cthc,
      n.at.cor=catn, n.ah.cor=cahn, n.th.cor=cthn)

    cbind(gc, gn,
          mca, mct, mch, mcm, mna, mnt, mnh,
          zca, zct, zch, zcm, zna, znt, znh,
          hca, hct, hch, hcm, hna, hnt, hnh,
          cor)
  })
  
  ## merge lists in a data.frame that contains uname, spot, id
  ftrs = do.call(rbind, mapply(function(f, i) data.frame(spot=i, id=1:nrow(f), f, stringsAsFactors=FALSE), ftrs, 1:length(ftrs), SIMPLIFY=FALSE))

  ## NAs to zero
  ftrs[is.na(ftrs)] = 0
  
  ftrs
}
