#' @rdname dpSet
#' @return \code{dpBatch()} returns a \code{dpBatch} object and saves the
#'   arguments passed to \code{dpBatch()} in a subdirectory of the associated
#'   set's directory.  The subdirectory is named according to the value in
#'   batchRecord that has a name equal to set$db$batchNameColumnName.
#'
dpBatch = function(batchRecord, set, dataValues, rawValues = data.frame()) {

  if (!is.list(batchRecord)) stop("batchRecord must be a named list of values.")
  if (!is.character(set) && !is.dpSet(set)) stop("setPath must be a dpSet object or a path (as a character string).")
  if (!is.data.frame(dataValues)) stop("dataValues must be a data.frame.")

  if(!is.dpSet(set)) set = dpLoadSet(set)

  if(set$db$batchRowColumnName %in% names(dataValues)) {
    stop("Don't include a column named '", set$db$batchRowColumnName, "' in the dataValues data frame.  Datapuppy will add the column automatically.")
#     # make sure user defined batch row numbers are unique...
#     if(anyDuplicated(dataValues[,set$db$batchRowColumnName])) {
#       stop("The column '", set$db$batchRowColumnName, "' in the dataValues argument contains duplicate values.")
#     }
#     # and that they are whole numbers
#     integerVals = as.integer(dataValues[,set$db$batchRowColumnName])
#     integerVals[is.na(integerVals)] = -1
#     if(any(dataValues[,set$db$batchRowColumnName] != integerVals)) {
#       stop("The column '", set$db$batchRowColumnName, "' in the dataValues argument must contain whole numbers.")
#     }
  } else {
    # add batch row numbers to the data
    dataValues =
      cbind(
        structure(
          list(seq(1:nrow(dataValues))),
          names = set$db$batchRowColumnName
          ),
        dataValues
        )
  }

  #check validity of data
  .dpValidateData(dataValues, set)

  #check to be sure that names in the batchRecord match the columns in the batchesTable
  requiredColumnsIdx = !(set$db$columns$batchesTableColumns %in% set$db$keys$batchesPrimaryKey)
  requiredColumnNames = set$db$columns$batchesTableColumns[requiredColumnsIdx]
  if(!identical(sort(names(batchRecord)), sort(requiredColumnNames))) {
    stop("Names of the values in the batchRecord list must be: ", paste0(requiredColumnNames, collapse = ", "))
  }

  connection = dpConnect(set$connectionArgs)

  batchName = as.character(batchRecord[set$db$batchNameColumnName])
  if(length(batchName) == 0) stop("The '", set$db$batchNameColumnName, "' value contained in the batchRecord list must contain a valid character string.")

  if(length(grep("[^[:alnum:]_-]", batchName))>0) {
    stop("Use only letters, numbers, dashes, or underscores in the '", set$db$batchNameColumnName, "' value contained in batchRecord list.")
  }

  #Test to be sure the batch name is unique...
  sqlCommand = paste0("SELECT COUNT(*) FROM ", set$db$tables$batchesTableName, " WHERE ", set$db$batchNameColumnName, " = '", batchName, "'")
  if (RODBC::sqlQuery(connection, sqlCommand)[,1] > 0) {
    stop("Batch name (from batchRecord argument) must be unique.  A batch named '", batchRecord[set$db$batchNameColumnName], "' already exists.")
  }

  # create path to batch directory
  batchPath = file.path(set$setPath, batchName)

  # make sure path doesn't refer to an file
  if(file_test("-f", batchPath)) {
    stop("'", batchPath,"' refers to an existing file.  It must refer to a non-existant directory or an empty directory.")
  }

  # if the directory exists, be sure it is empty
  if(file_test("-d", batchPath)) {
    if(length(list.files(batchPath))>0) {
      stop("A Datapuppy 'batch' can not be created in '", batchPath, "' because the directory exists and is not empty.")
    }
  # if it doesn't exist, create it
  } else {
    success = dir.create(batchPath, showWarnings = FALSE)
    if(!success) {
      stop("Could not create a folder called '", batchPath, "'.")
    }
  }

  committedDir = file.path(batchPath, "committed")
  success = dir.create(committedDir)
  if(!success) {
    stop("Could not create a folder called '", committedDir, "'.")
  }

#   rawDataDir = file.path(batchPath, "rawdata")
#   success = dir.create(rawDataDir)
#   if(!success) {
#     stop("Could not create a folder called '", rawDataDir, "'.")
#   }

  # put quotes around any string in the batchRecord
  quotedBatchRecord = lapply(batchRecord, function(x) if(is.character(x)) {paste0("'", x, "'")} else {x})

  # add the batch record to batchesTable
  sqlCommand = paste0("INSERT INTO ", set$db$tables$batchesTableName, " (", paste0(names(quotedBatchRecord), collapse = ", "), ") VALUES (", paste0(quotedBatchRecord, collapse = ", "), ")")
  RODBC::sqlQuery(connection, sqlCommand)

  RODBC::odbcClose(connection)

  # make a list of the arguments passed to this function
  argList = as.list(match.call())
  argList = c(argList[2:(length(argList))])
  argListNames = names(argList)

  # Copy the values into the argList.  We don't want to eval() here because a
  # promise with random number generation could be passed, so we want to get()
  # the values from memory.
  argList = sapply(argListNames, get, envir = environment(), simplify = F)

  newBatch = .dpBatchFromArgList(argList)

## NEED TO SAVE SOURCE DATA AND TWEAK SETS IN INCREMENTAL FILES, PLUS A
## DATAFRAME OF LOAD/UNLOAD EVENTS INDICATING THE SOURCES AND TWEAKS USED TO
## LOAD...  MAYBE MAKE A MASTER LIST OF ALL LOAD/UNLOAD EVENTS.  THEN TO RESTORE
## AS OF A CERTAIN DATE, JUST GET THE APPROPRIATE EVENT (LATEST EVENT BEFORE
## DESIRED DATE) FOR EACH BATCH.  MIGHT ALSO HAVE A PROVISION FOR BOOKMARKS.

  dpSaveBatchArgList(argList, newBatch$batchPath)

  return(newBatch)
}

dpLoadBatch = function(set, batchName) {
  if(!is.dpSet(set)) set = dpLoadSet(set)

  batchPath = .batchPath(set, batchName)
  argList = dpLoadBatchArgList(batchPath)
  return(.dpBatchFromArgList(argList))
}

.dpValidateData = function(dataValues, set) {

  if(!is.dpSet(set)) set = dpLoadSet(set)

  connection = dpConnect(set$connectionArgs)

  # SQL to select the datum types
  sqlCommand = paste("SELECT", set$db$datumTypeColumnName, "FROM", set$db$tables$typesTableName)
  datumTypes = as.character(RODBC::sqlQuery(connection, sqlCommand)[,1])

  RODBC::odbcClose(connection)

  # select all of the columns from the data table that are required input by the user
  # (e.g., not primary key, value, batchIDX or metricIDX)
  requiredColumnIdx = !(set$db$columns$dataTableColumns %in% calculatedDatumColumns(set))
  requiredColumns = set$db$columns$dataTableColumns[requiredColumnIdx]

  # compile list of allowed column names (e.g., required columns plus any datum type)
  allowedColumns = c(requiredColumns, datumTypes)

  submittedColumns = colnames(dataValues)

  missingColumnIdx = !(requiredColumns %in% submittedColumns)
  if(any(missingColumnIdx)) {
    stop("The following columns are missing from but required in the 'dataValues' data frame: ", paste(requiredColumns[missingColumnIdx], collapse = ", "))
  }

  illegalColumnIdx = !(submittedColumns %in% allowedColumns)
  if(any(illegalColumnIdx)) {
    stop("The following column names are not allowed: ",
         paste(submittedColumns[illegalColumnIdx], collapse = ", "),
         "\nLegal column names are:",
         paste(allowedColumns, collapse = ", ")
    )
  }

  if(length(requiredColumns) == length(submittedColumns)) {
    stop("The 'dataValues' data frame doesn't appear to have any columns that contain data to be loaded.")
  }

  return()
}

.dpBatchFromArgList = function(argList) {
  thisCall = do.call(call, c(list(".dpBatch"), argList))
  newBatch = eval(thisCall)
  return(newBatch)
}

.dpBatch = function(...) {

  # Examines ... argument and creates contained variables in this environment.
  .unpackDots(...)

  if(!is.dpSet(set)) set = dpLoadSet(set)

  # Error check to be sure names of batchRecord match column names in batch table.
  requiredColumnIdx = !(set$db$columns$batchesTableColumns %in% set$db$keys$batchesPrimaryKey)
  requiredColumns = sort(set$db$columns$batchesTableColumns[requiredColumnIdx])
  if(!identical(sort(names(batchRecord)), requiredColumns)) {
    stop("The 'batchRecord' must be a list containing ", length(requiredColumns),
         " value(s).  The names attribute of the list must be columns from the batch table, i.e.: ",
         paste0("'", requiredColumns, "'", collapse = ", "))
  }

  batchIDX = .batchIDX(set, batchRecord[[set$db$batchNameColumnName]])
  batchPath = .batchPath(set, batchRecord[set$db$batchNameColumnName])

  tweakFile = file.path(batchPath, "dpTweaks.rData")
  if(file_test("-f", tweakFile)) {
    load(file = tweakFile)
  } else {
    tweakList = list()
  }

  newBatch = list(
    batchIDX = batchIDX,
    batchRecord = batchRecord,
    batchPath = batchPath,
    setPath = set$setPath,
    rawBatchData = rawValues,
    batchData = dataValues,
    tweaks = tweakList
  )
  class(newBatch) = c("dpBatch")
  return(newBatch)
}

.batchPath = function(set, batchName) {
  if(!is.dpSet(set)) set = dpLoadSet(set)

# create path to batch directory
  return(file.path(set$setPath, batchName))
}

.batchIDX = function(set, batchName) {
  if(!is.dpSet(set)) set = dpLoadSet(set)

  # generate commands to retrieve or remove the inserted command.
  fromPartOfCommand = paste0("FROM ", set$db$tables$batchesTableName, " WHERE ", set$db$batchNameColumnName, " = '", batchName, "'")
  sqlCommand = paste("SELECT", set$db$keys$batchesPrimaryKey, fromPartOfCommand)

  # retrieve the batchIDX for the new batch
  connection = dpConnect(set$connectionArgs)
  batchIDX = RODBC::sqlQuery(connection, sqlCommand)[,set$db$keys$batchesPrimaryKey]
  RODBC::odbcClose(connection)
  return(batchIDX)
}

#' @rdname dpSet
#' @return \code{is.dpBatch()} returns TRUE if x is a \code{dpBatch} object.
is.dpBatch = function(x) {
  return(is(x, class2 = "dpBatch"))
}

