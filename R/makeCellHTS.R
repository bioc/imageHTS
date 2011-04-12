makeCellHTS = function(x, profiles, measurementNames, name) {
  if (!is.data.frame(profiles)) stop("'profiles' must a be a data frame containing the phenotypic profiles")
  if (ncol(profiles)>10) stop('too many features (columns) in phenotypic profiles')
  if (!'uname'%in%colnames(profiles)) stop("'profiles' must contain the 'uname' field")
  prw = uname2prw(profiles$uname)
  well = rowcol2well(prw$row, prw$col)
  profiles$uname = NULL
  xd = data.frame(plate=prw$plate, replicate=prw$replicate, well=well, profiles)
  if (missing(measurementNames)) y = buildCellHTS2(xd)
  else y = buildCellHTS2(xd, measurementNames)
  if (!missing(name)) name(y) = name
  y
}
