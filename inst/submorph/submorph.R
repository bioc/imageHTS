submorph = function() {
  ## load imageHTS
  library('imageHTS')
  
  ## initialize imageHTS object
  localPath = tempdir()
  serverURL = system.file('submorph', package='imageHTS')
  x = parseImageConf('conf/imageconf.txt', localPath=localPath, serverURL=serverURL)
  x = configure(x, 'conf/description.txt', 'conf/plateconf.txt', 'conf/screenlog.txt')
  x = annotate(x, 'conf/annotation.txt')

  ## get non-empty wells
  unames = setdiff(getUnames(x), getUnames(x, content='empty'))
  
  ## segment and extract features
  segmentWells(x, unames, 'conf/segmentationpar.txt')
  extractFeatures(x, unames, 'conf/featurepar.txt')

  ## classification
  readLearnTS(x, 'conf/featurepar.txt', 'conf/trainingset.txt')
  predictCellLabels(x, unames)
  
  ## summarize features
  profiles = summarizeWells(x, unames, 'conf/featurepar.txt')

  ## install web modules
  installCellPicker(x)
  installWebQuery(x)

  ## test modules
  popCellPicker(x, unames)
  popWebQuery(x)
}
