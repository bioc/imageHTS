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
