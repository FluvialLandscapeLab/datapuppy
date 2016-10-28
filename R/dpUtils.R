# Save and load a datapuppy argList
#
# Rather than saving \code{dp Objects} to disk, \code{datapuppy} saves the
# arguments passed to the constructor fuctions. Thus, when a \code{dp Object}
# is loaded from disk, \code{datapuppy} acually loads the argument list and
# regnerates the object by calling the constructor function.
# @return .dpSaveArgList() Returns \code{TRUE} if successful.  Throws error if
#   not successful.
# @param argList the argument list used by the constructure function
# @param path the directory where the file that stores the argList is to be
#   saved
# @param dpType a character vector (of length() == 1)  saying the type of
#   \code{dp object} to be saved (e.g., "dpSet", "dpBatch").  Combined with
#   \code{path} arguement, this value is used as the name of the ".rData" file
#   where the arguments are stored.  Also, by definition, this string is the
#   name of the constructor function to which the \code{argList} can be passed.
# @param validate is a \code{boolean} determining whether the \code{list} is to
#   be validated against the formals of the constructor function.
.dpSaveArgList = function(argList, path, dpType, validate) {
  fileName = dpCheckPath(path, paste0(dpType, ".rData"))
  if(validate) {
    .dpValidateArgListNames(argList, dpType, reportError = TRUE)
  }
  save(argList, file = fileName)
  return(TRUE)
}

# @return .dpLoadArgList returns a list of arguements that can be passed to a
#   dp constructor function.
.dpLoadArgList = function(path, dpType, validate = TRUE) {
  path = dpCheckPath(path)
  fileName = dpCheckPath(path, paste0(dpType, ".rData"))
  load(file = fileName)
  if(validate) {
    .dpValidateArgListNames(argList, dpType, reportError = TRUE)
  }
  return(argList)
}


# @details .dpValidateArgListNames() Compares argList to the results of formals
#   of function specified by "functionName.
# @return If reportError is TRUE, function throws an error and stops execution
#   when argList doesn't supply the correct arguments for the function.  If
#   FALSE, errors are returned in a list.  See \code{\link{dpCheckSet}()} for
#   explanation of error list.
.dpValidateArgListNames = function(argList, functionName, reportError = TRUE) {
  # get arguments from the dpSet
  argNames = sort(names(argList))

  # investigate arguments required by current implementation of dpSet
  formalNames = sort(names(formals(functionName)))

  # compare argument sets and report missing or extra values.
  badValues = list(missingArguments = "", extraArguments = "")
  errorMsg = ""
  if(!identical(argNames, formalNames)) {
    badValues[[1]] = formalNames[!(formalNames %in% argNames)]
    badValues[[2]] = argNames[!(argNames %in% formalNames)]
    if(length(badValues[[1]])>0) {
      errorMsg = paste0("The following arguments are missing from the stored ", functionName,": ", paste0(badValues[[1]], collapse = ", "), "\n  ")
    }
    if(length(badValues[[2]])>0) {
      errorMsg = paste0(errorMsg, "The following extra arguments should be removed from the stored ", functionName, ": ", paste0(badValues[[2]], collapse = ", "))
    }
  }
  if(reportError) {
    if(nchar(errorMsg)>0) {
      stop(errorMsg)
    }
#     else {
#       cat(paste0("CHECK COMPLETE: names of argument list match formals(", functionName, ")\n\n"))
#     }
  }
  return(badValues)
}

#' Validate a path
#'
#' Converts a user-specified path to an normalised, absolute path (see
#' \code{\link{normalizePath}}. If the user-specified path is a relative path,
#' the path is assumed to be relative to the R working directory (use
#' \code{\link{getwd}} to see the working directory). Checks to be sure the
#' normalized  path refers to an existing directory. Optionally appends a
#' fileName to the path, if specified.
#' @param path a user-specified path, appropriate for the platform upon which R
#'   is running.
#' @param fileName = a file name to be appended to the end of the path.
#' @return A validated absolute path with any specified file appended.
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

# examine ... argument and creates variables in calling environment.
.unpackDots = function(...) {
  vals = list(...)
  varnames = names(vals)
  for (i in 1:length(vals)) {
    assign(varnames[i], vals[[i]], envir = parent.frame())
  }
}

#' Auto-generated columns for any dpBatch in a dpSet
#'
#' @description For all \code{dpBatches} in a \code{dpSet}, \code{Datapuppy} automatically
#' generates some columns prior to loading the \code{dpBatchs}'s data into the
#' database associated with the \code{dpSet}.
#'
#' @return Returns a character vector of the columns that are automatically
#'   created by \code{datapuppy} for any given \code{dpSet}.  These columns
#'   appear in one or more tables in the database associated with the
#'   \code{dpSet}, but should not (and can not) be specified as a column name in
#'   any data.frame assciated with a \code{dpBatch}.
#' @param set A \code{dpSet} object or the path to a \code{dpSet} directory on
#'   disk.
calculatedDatumColumns = function(set) {
  if(!is.dpSet(set)) set = dpLoadSet(set)

  c(set$db$keys$dataPrimaryKey,
    set$db$keys$batchesPrimaryKey,
    set$db$keys$typesPrimaryKey,
#    set$db$batchRowColumnName,
    set$db$datumValueColumnName
  )
}

# looks at a dataframe and converts any POSIXt or Date columns to character.
.dpDateColToCharacter = function(DF) {
  dateColumns = which(sapply(DF, function(x) {inherits(x, "POSIXt") || inherits(x, "Date")}))
  dateColumns = names(DF)[dateColumns]
  for(theCol in dateColumns) {
    DF[,theCol] = format(DF[,theCol])
  }
  return(structure(DF, dateColumns = dateColumns))
}

# returns a list of the subset of formals (generated with
# \code{\link{formals}()}) that don't have defaults, and thus are required.
.formalsWithoutDefaults = function(listOfFormals) {
  hasNoDefaultIdx =
    sapply(
      listOfFormals,
      function (x) {
        hasNoDefault = F
        if (class(x) == "name") {
          if(nchar(x) == 0) {
            hasNoDefault = T
          }
        }
        return(hasNoDefault)
      }
    )
  return(listOfFormals[hasNoDefaultIdx])
}


UTCTime = function(targetTime = Sys.time()) {
  return(format(targetTime, tz="UTC", usetz = TRUE))
}

.dpOption = function(name) {
  fullName = paste0("datapuppy.", name)
  return(options(fullName)[[fullName]])
}

.dpTransferBatch = function(batch) {

  set = dpLoadSet(batch$setPath)

  connection = dpConnect(set$connectionArgs)

  if(class(connection) == "MySQLConnection") {

    tempFileName = normalizePath(tempfile(), "/", mustWork = F)

    dpGetQuery(
      connection,
      paste("DELETE FROM", set$db$tables$dataTableName, "WHERE", set$db$keys$batchesPrimaryKey, "=", batch$batchIDX )
    )

    write.table(batch$batchData, file = tempFileName, row.names = F, col.names = F, sep = "\t", quote = F)
    sqlResult = dpGetQuery(
      connection,
      paste0(
        "LOAD DATA LOCAL INFILE '", tempFileName, "' ",
        "INTO TABLE ", set$db$tables$dataTableName,
        " (", paste0(names(batch$batchData), collapse = ", "), ")")
    )

    file.remove(tempFileName)
    if(length(sqlResult) > 0) stop(sqlResult)
  } else {
    sqlAppendTable(connection, dbQuoteIdentifier(connection, set$db$tables$dataTableName), batch$batchData)
#    stop("Unexpected connection type '", class(connection), "' in .dpTransferBatch() function.  Datapuppy will need to be fixed.  Please contact the maintainer of the datapuppy package.")
  }

}

.dpValidConnection = function (connection) {
  if(!(class(connection) %in% .dpOption("connections"))) {
    stop(
      "A dbi connection of class '",
      class(connection),
      "' is not currently supported by datapuppy.  Supported connection classes are: ",
      paste(.dpOption("connections"), collapse = ", "),
      ".")
  }
}
