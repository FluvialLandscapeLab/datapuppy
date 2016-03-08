#' Converts path to absolute path, checks to be sure it's an existing directory.
#' Optionally appends a fileName, if specified
dpCheckPath = function(path, fileName = "") {
  fullPath = normalizePath(path, "/", FALSE)
  if(file_test("-f", fullPath)) {
    stop("'", path, "' appears to refer to a file.  It must be a directory.")
  }
  if(!file_test("-d", fullPath)) {
    stop("'", path, "' is not the name of an existing directory.")
  }

  if(nchar(fileName)>0) {
    fullPath = file.path(fullPath, fileName)
  }
  return(fullPath)
}

#' examine ... parameter and creates variables in calling environment.
unpackDots = function(...) {
  vals = list(...)
  varnames = names(vals)
  for (i in 1:length(vals)) {
    assign(varnames[i], vals[[i]], envir = parent.frame())
  }
}

calculatedDatumColumns = function(set) {
  c(set$db$keys$dataPrimaryKey,
    set$db$keys$batchesPrimaryKey,
    set$db$keys$typesPrimaryKey,
    set$db$batchRowColumnName,
    set$db$datumValueColumnName
  )
}
