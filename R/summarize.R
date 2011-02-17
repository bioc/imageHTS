summarizeWells = function(x, uname, featurePar, access='cache') {
  p = readHTS(x, type='file', filename=featurePar, access=access, format='dcf')
  
  profiles = as.list(rep(NA, length(uname)))
  names(profiles) = uname
  for (i  in 1:length(uname)) {
    u = uname[i]
    cat(u, ': ', sep='')
    ftrs = try(readHTS(x, 'ftrs', uname=u, access=access))
    if (class(ftrs)!='try-error') {   
      ## compute median ftrs
      ftrs$spot = NULL
      ftrs$id = NULL
      n = nrow(ftrs)
      medianftrs = try(apply(ftrs, 2, median), silent=TRUE)
      
      ## if all features are NA (e.g. in case of black image)
      if (class(medianftrs)=='try-error') prof = NA
      else {
        names(medianftrs) = paste('med.', names(medianftrs), sep='')
         
        if (!is.null(p$cell.classes)) {
          ## add class fractions
          cfrac = rep(0, length(p$cell.classes))
          names(cfrac) = p$cell.classes
          
          ## compute cell class fractions
          f = fileHTS(x, 'clabels', uname=u ,access=access)
          if (file.exists(f)) {
            clabels = readHTS(x, 'clabels', uname=u ,access=access)$label
            z = table(clabels)
            cfrac[names(z)] = z/n         
          } else {
            msg = paste('cannot find the file that contains class labels for well=', u, '\n', sep='')
            msg = paste(msg, '  maybe predictCellLabels has not be called ?\n', sep='')
            msg = paste(msg, '  maybe the field \'cell.classes\' in the feature parameters file should be empty ?\n', sep='')
            stop(msg)
          }
          
          prof = c(n=n, medianftrs, cfrac)
        }
        else prof = c(n=n, medianftrs)
      }
      
      profiles[[i]]= prof
      cat('OK\n')
    } else cat('NA\n')
  }

  profiles = do.call(rbind, profiles)
  profiles = data.frame(uname=uname, profiles, stringsAsFactors=FALSE)
  rownames(profiles) = NULL
 
  if (all(is.na(profiles[,1]))) stop('no cell features found, no profiles generated.')
  else {
    ff = fileHTS(x, type='file', filename='data/profiles.tab', createPath=TRUE, access='local')
    write.table(profiles, file=ff, sep='\t', quote=FALSE, row.names=FALSE, col.names=TRUE)
  }
  
  invisible(profiles)
}

collectCellFeatures = function(x, uname, spot=NULL, id=NULL, access='cache') {
  if (!is.null(spot)) stopifnot(length(spot)==length(uname))
  if (!is.null(id)) stopifnot(!is.null(spot) & length(spot)==length(uname))

  su = split(1:length(uname), uname)
  ftrs = lapply(1:length(su), function(i) {
    u = names(su)[i]
    ind = unique(cbind(spot=spot[su[[i]]], id=id[su[[i]]]))
    cat(u, ': ', sep='')
    ## read features
    ftrs = try(readHTS(x, 'ftrs', uname=u, access=access))
    if (class(ftrs)!='try-error') {
      if (!is.null(id)) {
        ftrs = merge(ftrs, ind, by=c('spot', 'id'), all.y=TRUE)
      }
      cat('OK\n')
      if (nrow(ftrs)>0) data.frame(uname=u, ftrs, stringsAsFactors=FALSE)
      else data.frame(uname=character(0), ftrs, stringsAsFactors=FALSE)
    }                   
    else {
      cat('NA\n')
      NULL
    }
  })
  ftrs = do.call(rbind, ftrs)

  ## order according to the original query
  if (!is.null(id)) {
    ftrs = merge(ftrs, cbind(uname=uname, spot=spot, id=id, index=1:length(uname)), by=c('uname', 'spot', 'id'))
    ftrs = ftrs[order(as.numeric(as.character(ftrs$index))),]
    ftrs$index = NULL
    rownames(ftrs) = NULL
  }

  ## remove factors
  ftrs$uname = as.character(ftrs$uname)
  ftrs
}
