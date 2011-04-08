make.crown = function(diam, thickness=6) {
  d1 = makeBrush(diam, 'disc')
  dk = makeBrush(diam-thickness, 'disc')
  d4 = array(0, dim=dim(d1))
  z = round((dim(d1)-dim(dk))/2)
  d4[z[1] + 1:nrow(dk), z[1] + 1:nrow(dk)] = dk
  d1 - d4
}

extractCells = function(a, ga, diameters, overlap) {
  ma = array(0, dim=dim(a))
  cells = matrix(NA, nrow=0, ncol=3, dimnames=list(NULL, c('x', 'y', 'radius')))
  for (i in dim(ga)[3]:1) {
    r = round(diameters[i]/2)
    cs = which(ga[,,i]>0, 2)
    if (nrow(cs)>0) {
      for (j in 1:nrow(cs)) {
        c = cs[j,]
        if (ma[c[1], c[2]]==0) {
          ma = drawCircle(ma, c[1], c[2], r+overlap, 1, fill=TRUE)
          cells = rbind(cells, c(c[1], c[2], r))
        }
      }
    }
  }
  cells
}

segmentRing = function(a, p) {
  nseg = array(0, dim=dim(a))
  cseg = array(0, dim=dim(a))
  
  if (is.null(p$no.segmentation)) {
    ## extract edges
    za = abs(a - 0.5)>p$edge.threshold
    za = za - opening(za, makeBrush(as.numeric(p$max.membrane.thickness), 'disc'))
    
    ## project g
    diameters = do.call(seq, as.list(as.numeric(p$crown.steps)))
    g = array(0, dim=c(dim(a), length(diameters)))
    for (i in 1:length(diameters)) {
      wf = make.crown(diameters[i],as.numeric(p$crown.thickness))
      wf = wf/sum(wf)
      z = filter2(za, wf)
      g[,,i] = z
    }
    
    ## extract local minima
    w = as.numeric(p$locmin.threshold.width)
    g2 = thresh(g, w=w, h=w, offset=as.numeric(p$locmin.threshold.offset))
    g2 = erode(g2, makeBrush(as.numeric(p$locmin.erode.size), 'disc'))
    g3 = array(0, dim=dim(g2))
    for (i in 2:dim(g3)[3]) g3[,,i] = g2[,,i-1] & g2[,,i]
    cells = extractCells(a, g3, diameters=diameters, overlap=as.numeric(p$cell.max.overlap))
     
    ## build segmentation masks
    if (nrow(cells)>0) {
      for (i in 1:nrow(cells)) {
        x = cells[i, 'x']
        y = cells[i, 'y']
        r = cells[i, 'radius']
        nseg = drawCircle(nseg, x, y, r-as.numeric(p$nucleus.radius.offset), i, fill=TRUE)
        cseg = drawCircle(cseg, x, y, r-as.numeric(p$cell.radius.offset), i, fill=TRUE)
      }
    }
    ## display(highlightSegmentation(a, nseg, cseg))
  }
  
  list(cseg=cseg, nseg=nseg)
}

segmentYeastBF = function(x, uname, p, access) {
  sbf = NULL
  nseg = NULL
  cseg = NULL
  spots = getImageConf(x)$SpotNames
  if (is.null(spots)) nbspots = 1
  else nbspots = length(spots)
  for (spot in 1:nbspots) {
    cat(spot)
    f1 = fileHTS(x, 'source', uname=uname, channel=1, spot=spot, access=access)
    if (!file.exists(f1)) stop('Missing source files: cannot open ', f1)
    
    ## read and segment bright field
    bf = readImage(f1)
    seg = segmentRing(bf, p)
    if (is.null(sbf)) {
      sbf = bf
      nseg = seg$nseg
      cseg = seg$cseg
    }
    else {
      sbf = EBImage::combine(sbf, bf)
      nseg = EBImage::combine(nseg, seg$nseg)
      cseg = EBImage::combine(cseg, seg$cseg)
    }
  }

  ## final image based on GFP
  cat(' nbcells=', countObjects(cseg), sep='')
  list(cal=sbf, nseg=nseg, cseg=cseg)
}
