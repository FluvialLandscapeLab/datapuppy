#' Create and manage batches
#'
#' These are details.
#'
#' @param batchRecord A named \code{list} of values describing the \code{batch}.
#'   Values in the \code{batchRecord} are stored as a record in the batches
#'   table.  \code{Names} attribute of \code{batchRecord} must contain the name
#'   of the database table column where each value is to be stored.
#' @param set A \code{\link{dpSet}} object describing \code{set} to which the
#'   batch will be added.
#' @param dataValues A \code{data.frame} with the values that are to be loaded
#'   into the database.
#'
dpBatch = function(batchRecord, setPath, dataValues) {

  if (!is.list(batchRecord)) stop("batchRecord must be a named list of values.")
  if (!is.character(setPath)) stop("setPath must be a character string.")
  if (!is.data.frame(dataValues)) stop("dataValues must be a data.frame.")

  set = dpLoadSet(batch$setPath)

  #check to be sure that names in the batchRecord match the columns in the batchesTable
  requiredColumnsIdx = !(set$db$columns$batchesTableColumns %in% set$db$keys$batchesPrimaryKey)
  requiredColumnNames = set$db$columns$batchesTableColumns[requiredColumnsIdx]
  if(!identical(sort(names(batchRecord)), sort(requiredColumnNames))) {
    stop("Names of the values in the batchRecord list must be: ", paste0(requiredColumnNames, collapse = ", "))
  }

  connection = dpConnect(set$connectionParams)

  batchName = as.character(batchRecord[set$db$batchNameColumnName])
  if(length(batchNames) = 0) stop("The '", set$db$batchNameColumnName, "' value contained in batchRecord list must contain a valid character string.")

  if(length(grep("[^[:alnum:]_-]", batchName))>0) {
    stop("Use only letters, numbers, dashes, or underscores in the '", set$db$batchNameColumnName, "' value contained in batchRecord list.")
  }

  #Test to be sure the batch name is unique...
  sqlCommand = paste0("SELECT COUNT(*) FROM ", set$db$tables$batchesTableName, " WHERE ", set$db$batchNameColumnName, " = '", batchName, "'")
  if (RODBC::sqlQuery(connection, sqlCommand)[,1] > 0) {
    stop("Batch name (from batchRecord parameter) must be unique.  A batch named '", batchRecord[set$db$batchNameColumnName], "' already exists.")
  }

  # create path to batch directory
  batchPath = file.path(setPath, batchName)

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

  # put quotes around any string in the batchRecord
  batchRecord = lapply(batchRecord, function(x) if(is.character(x)) {paste0("'", x, "'")} else {x})

  # add the batch record to batchesTable
  sqlCommand = paste0("INSERT INTO ", set$db$tables$batchesTableName, " (", paste0(names(batchRecord), collapse = ", "), ") VALUES (", paste0(batchRecord, collapse = ", "), ")")
  RODBC::sqlQuery(connection, sqlCommand)

  # generate commands to retrieve or remove the inserted command.
  fromPartOfCommand = paste0("FROM ", set$db$tables$batchesTableName, " WHERE ", set$db$batchNameColumnName, " = ", batchRecord[set$db$batchNameColumnName])
  undoCommand = paste0("DELETE * ", fromPartOfCommand)
  sqlCommand = paste0("SELECT ", set$db$keys$batchesPrimaryKey, fromPartOfCommand)

  # retrieve the batchIDX for the new batch
  batchIDX = RODBC::sqlQuery(connection, sqlCommand)[,1]


## SHOULD I DO THE SAME THING AND SAVE THE PARAMS THAT CREATE THE BATCH?  IT
## SEEMS THIS WOULD ALLOW THE DATA TO BE RECREATED FOR A PAST POINT IN TIME EVEN
## IF THE DATABASE SCHEMA CHANGES OVER TIME...

## NEED TO SAVE SOURCE DATA AND TWEAK SETS IN INCREMENTAL FILES, PLUS A
## DATAFRAME OF LOAD/UNLOAD EVENTS INDICATING THE SOURCES AND TWEAKS USED TO
## LOAD...  MAYBE MAKE A MASTER LIST OF ALL LOAD/UNLOAD EVENTS.  THEN TO RESTORE
## AS OF A CERTAIN DATE, JUST GET THE APPROPRIATE EVENT (LATEST EVENT BEFORE
## DESIRED DATE) FOR EACH BATCH.  MIGHT ALSO HAVE A PROVISION FOR BOOKMARKS.

  newBatch = list(
    batchIDX = batchIDX,
    batchRecord = batchRecord,
    batchPath = batchPath
  )
  class(newBatch) = c("dpBatch")

  fileName = dpCheckPath(batchPath, "dpBatch.rData")

  return(newBatch)
}

.dpValidateData = function(batch) {

  set = dpLoadSet(batch$setPath)

  # SQL to select the datum types
  sqlCommand = paste(
    "SELECT", batch$set$db$datumTypeColumnName,
    "FROM", batch$set$db$typesTableName
  )

  datumTypes = as.character(RODBC::sqlQuery(connection, sqlCommand)[,1])

  # select all of the columns from the data table that are required input by the user
  # (e.g., not primary key, value, batchIDX or metricIDX)
  requiredColumnIdx = !(set$db$columns$dataTableColumns %in% calculatedDatumColumns(batch$set))
  requiredColumns = set$db$columns$dataTableColumns[requiredColumnIdx]

  # compile list of allowed column names (e.g., required columns plus any datum type)
  allowedColumns = c(requiredColumns, datumTypes)

  submittedColumns = colnames(dataValues)

  # some error checking
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

}

.dpBatch = function(...) {

  # Examines ... parameter and creates contained variables in this environment.
  unpackDots(...)

  # Error check to be sure names of batchRecord match column names in batch table.
  requiredColumnIdx = !(set$db$columns$batchesTableColumns %in% set$db$keys$batchesPrimaryKey)
  requiredColumns = sort(set$db$columns$batchesTableColumns[requiredColumnIdx])
  if(!identical(sort(names(batchRecord)), requiredColumns)) {
    stop("The 'batchRecord' must be a list containing ", length(requiredColumns),
         " values.  The names attribute of the list must be columns from the batch table, i.e.: ",
         paste0("'", requiredColumns, "'", collapse = ", "))
  }

}
