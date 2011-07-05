readLearnTS = function(x, featurePar, trainingSet, access='cache', cost, gamma) {
  if (missing(cost)) cost = c(0.1, 1, 10, 20)
  if (missing(gamma)) gamma = c(0.0001, 0.001, 0.01, 0.1)
  
  p = readHTS(x, type='file', filename=featurePar, access=access, format='dcf')
  ts = readHTS(x, type='file', filename=trainingSet, access=access, format='tab')
  xts = collectCellFeatures(x, uname=ts[,1], spot=ts[,2], id=ts[,3], access=access)
  yts = factor(ts[,4])
  cat('Read',nrow(xts),'cell features.\n')
  cat('TS cell classes distribution:\n')
  print(table(yts))

  ## remove features undesired for classifcation
  z = p$remove.classification.features
  if (!is.null(p)) {
    iz = match(z, colnames(xts))
    xts = xts[,-iz]
  }
  xts$uname = NULL
  xts$spot = NULL
  xts$id = NULL

  ## sanity check
  if (sum(is.na(xts))>0) stop('cell features contains NA values !')

  ## tune SVM
  cat('Training using SVM... This could take some time.\n')
  tu = tune(svm, xts, yts, ranges = list(gamma=gamma, cost=cost))
  cellClassifier = list(classifier=tu$best.model, cft=colnames(xts))
  print(tu)
  
  ## print TS confusion table
  ytsp = predict(cellClassifier$classifier, xts)
  print(table(yts,ytsp))
  
  ## save classifier
  fc = fileHTS(x, type='file', filename='data/classifier.rda', createPath=TRUE, access='local')
  save(cellClassifier, file=fc)
  invisible(cellClassifier)
}

predictCellLabels = function(x, uname, access='cache') {
  cellClassifier = try(readHTS(x, type='file', filename='data/classifier.rda', access=access, format='rda'))
  if (class(cellClassifier)=='try-error') stop("cannot open cell classifier data, please run readLearnTS first")
  
  for (u in uname) {
    cat(u, ': ', sep='')
    ftrs = try(readHTS(x, 'ftrs', uname=u, access=access))
    if (class(ftrs)=='try-error') cat(' KO\n')
    else {
      ## predict
      xd = ftrs[, cellClassifier$cft]

      ## sanity check
      if (sum(is.na(xd))>0) stop('cell features contains NA values !')
      
      clabels = as.character(predict(cellClassifier$classifier, xd))
      z = table(clabels)
      cat(paste(names(z), z, sep='=', collapse=' '))
      clabels = cbind(ftrs[,c('spot', 'id')], label=clabels)

      ## save label information
      ff = fileHTS(x, 'clabels', uname=u, createPath=TRUE, access='local')
      write.table(clabels, file=ff, sep='\t', quote=FALSE, row.names=FALSE, col.names=TRUE)
      
      ## save Javascript labels for cellPicler
      for (i in unique(clabels$spot)) {
        ff = fileHTS(x, type='jsclabels', uname=u, spot=i, createPath=TRUE, access='local')
        con = file(ff, "wt")
        write('cellLabels = new Array();',con)
        z = clabels$spot == i
        write(paste('cellLabels[', 1:length(clabels$label[z]), '] = "', as.character(clabels$label[z]), '"', sep=''), con, append=TRUE)
        close(con)
      }
      cat(' OK\n')
    }
  }
}

test.pop
