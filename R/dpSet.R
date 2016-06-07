#' Create and manage datapuppy \code{sets} and \code{batches}
#'
#' Datapuppy \code{Batches} are collections of records derived from a single
#' data file (e.g., a data file downloaded from a data logger or a spreadsheet
#' that contains field observations).  Datapuppy \code{sets} are collections of
#' data \code{batches} to be loaded into a single database.
#'
#' A \code{batch} is a collection of data points that have been imported from a
#' single data file.  For instance, when a data logger is downloaded, it creates
#' a data file with many records, where each record may contain observations of
#' several different metrics. All (or a subset) of those observations can be
#' collected into a \code{batch}.
#'
#' A \code{set} is a collections of data \code{batches}.  A \code{set} is a
#' collection of all of the \code{batches} stored in a particular database.
#' Thus, each \code{set} is always associated with a single database.

#' \code{dpSets} and \code{dpBatches} are S3 objects built atop \code{lists}.
#' \code{dpSet} objects contains information about a \code{set} and the database
#' associate with the \code{set}.  \code{dpBatch} objects contain information
#' about the \code{batch} and the \code{set} to which the \code{batch} belongs.
#'
#' Because \code{dpSets} (and \code{dpBatches}) are \code{lists}, information in a
#' \code{dpSet} (\code{dpBatch}) can be accessed with the $ operator.  For
#' instance, given a \code{set} named \code{mySet},
#' \code{mySet$db$keys$dataPrimaryKey} would return the name of the primary key
#' column for the data table in the database. Generally, tho, the user should
#' not have to investigate the contents of a \code{dpSet} (\code{dpBatch})
#' object. Instead, call \code{dpSet()} (\code{dpBatch}) to create the S3
#' object, store the object in a variable, and pass the variable to other
#' functions.
#'
#' Note that datapuppy makes a few assumptions about the database into which
#' batches are loaded.  There must be a "batch" table where each record contains
#' infomation about a \code{batch} loaded into the database.  There must be a
#' "data" table where each record is a single datum.  And there must be a
#' "types" table where each record describes a metric that can be assoicated
#' with any datum (e.g., the datum represents a temperature reading, a wind
#' speed, a stock price, or whatever metrics are traked by the database). The
#' columns in the \code{batchesTable}, \code{dataTable}, and \code{typesTable}
#' of the database have some requirements:
#'
#' 1) Each table must contain an autonumber field that is designated as the
#' primary key for the table
#'
#' 2) the \code{dataTable} must contain at least two foreign keys, one that
#' refers to and is named the same as the primary key column of the
#' \code{batchesTable}, and one that refers to and is named the same as the
#' primary key column of the \code{typesTable}.  In this way, each datum is
#' associated with a \code{batch} that describes the source of the datum and
#' with a \code{datatype} that describes what the number represents.
#'
#' @details \code{dpSet()} (\code{dpBatch()}) creates a file called
#'   "dpSet.rData" ("dpBatch.rData") in the location specified by \code{setPath}
#'   (\code{\link{file.path}(set$setPath, batchRecord$batchName)}). The file
#'   contains a list of the arguments passed to \code{dpSet()}
#'   (\code{dpBatch()}).  When a \code{set} (\code{batch}) is reloaded from disk
#'   using \code{dpLoadSet()} (code{dpLoadBatch()}), the list of the arguments
#'   is used to recreate the \code{dpSet} (\code{dpBatch}).
#'
#' @param setPath A character string containing the path to the \code{set} (\code{batch})
#'   directory.  This directory contains the \code{"dpSet.rData"} (]code{"dpBatch.rData}) file and a subfolder
#'   for each \code{batch} included in the \code{set}.  The folder must exist and be
#'   empty when a \code{set} is created using \code{dpSet()}.  If passed to
#'   other functions as a 'set' argument, the folder specified by \code{setPath}
#'   must contain a 'dpSet.rData' file.  If a path is not fully specified, it is
#'   assumed to be a subdirectory of the R working directory (see
#'   \code{\link{getwd}}).
#' @param batchPath same as setPath, above.
#' @param validate A boolean determining whether names in the argument list are
#'   validated against \code{\link{formals}(dpSet)} or
#'   \code{\link{formals}(dpBatch)}.
#' @param conndectionArgs A \code{dpConnectionArgs} object returned from
#'   \code{\link{dpConnectionArgs()}} describing the location and credentials
#'   for the database associated with a \code{set}.
#' @param batchRowColumnName A character string containing the name of a column
#'   in the \code{dataTable} of the database; the column is used to store the
#'   row number of the \code{dataValues data.frame} that was the source of the
#'   datum.
#' @param batchNameColumnName A character string containing the name of a column
#'   in the \code{batchTable} of the database that stores a unique name for the
#'   batch.  This name should be meaningful to a human to identify the batch,
#'   not the serial number (primary key) assigned to the batch automatically by
#'   datapuppy.
#' @param batchesTableName A character string containing the name of the
#'   \code{batchesTable} in the database.
#' @param dataTableName A character string containing the name of the
#'   \code{dataTable} in the database.
#' @param typesTableName A character string containing the name of the
#'   \code{typesTable} in the database.
#' @param batchRecord A named \code{list} of values describing the \code{batch}.
#'   Values in the \code{batchRecord} are stored as a record in the batches
#'   table. \code{Names} attribute of \code{batchRecord} must contain the name
#'   of the database table column where each value is to be stored.
#' @param set A \code{\link{dpSet}} object describing the \code{set} to be
#'   operated on (or the \code{set} to which a batch will be added).
#'   Alternatively, a setPath (see 'setPath' argument in \code{\link{dpSet}})
#'   from which a set will be loaded.
#' @param dataValues A \code{data.frame} with the values that are to be loaded
#'   into the database.
#' @param x An object to be tested.
#' @return \code{dpSet()} (\code{dpBatch()} returns a \code{dpSet}
#'   (\code{dpBatch}) object that describes the \code{set} (\code{batch}). This
#'   object should be assigned to a variable so that it can be passed to other
#'   datapuppy functions.  The arguments passed to \code{dpSet()}
#'   (\code{dpBatch()}) are also saved in a file on disk (see Details, above).
#' @export
dpSet = function(
  setPath,
  connectionArgs,
  batchRowColumnName,
  datumValueColumnName,
  datumTypeColumnName,
  batchNameColumnName,
  batchesTableName,
  dataTableName,
  typesTableName) {

  setPath = dpCheckPath(setPath)
  if(length(list.files(setPath))>0) {
    stop("A Datapuppy 'set' can not be created in '", setPath, "' because the folder is not empty.  Please choose an empty folder to create a new 'set.'")
  }

  # make a list of the arguments passed to this function
  argList = as.list(match.call())
  argList = argList[2:(length(argList))]

  newSet = .dpSetFromArgList(argList)

  #The argument list generated by as.list(match.call()) puts the name of the
  #connectionArgs object in the list of arguments, rather than the object
  #itself. So we put the object in the argument list before saving it to
  #disk.
  argList$connectionArgs = connectionArgs

  #Save the arguments to disk so that we can regenerate the set at any time using
  #dpLoadSet().
  dpSaveSetArgList(argList)
  return(newSet)
}

#' @rdname dpSet
#' @return \code{dpLoadSet()} (\code{dpLoadBatch()}) creates a \code{dpSet}
#' (\code{dpBatch}) object described by the arguments stored in the
#' "dpSet.rData" ("dpBatch.rData") file.
dpLoadSet = function(setPath) {
  argList = dpLoadSetArgList(setPath)
  return(.dpSetFromArgList(argList))
}


# Checks for some basic agreement between set and the database schema
.dpValidateSet = function(set) {

# an internal fuction for reporting a missing column
  checkForMissingColumn = function(columnName, columnNames, tableName) {
    if (!(columnName %in% columnNames)) {
      stop(
        "'",
        columnName,
        "' must be a column in the table '",
        tableName,
        ".'\nThe columns in the table are: ",
        paste(columnNames, collapse = ", "),
        "."
      )
    }
  }

  connection = dpConnect(set$connectionArgs)

  # check to be sure all tables specified in the set are in the database
  existingTables = RODBC::sqlTables(connection)$TABLE_NAME
  missingTables = !(set$db$tables %in% existingTables)
  if (any(missingTables)) {
    RODBC::odbcClose(connection)
    stop("The following requested tables do not exist in the database: ",
         paste(set$db$tables[missingTables], collapse = ", "), "\n",
         "Existing tables are:",
         paste(existingTables, collapse = ", ")
    )
  }

  RODBC::odbcClose(connection)

  # ensure batchRowColumn is in dataTable
  checkForMissingColumn(
    set$db$batchRowColumnName,
    set$db$columns$dataTableColumns,
    set$db$tables$dataTableName
  )

  # ensure datumValueColumn is in dataTable
  checkForMissingColumn(
    set$db$datumValueColumn,
    set$db$columns$dataTableColumns,
    set$db$tables$dataTableName
  )

  # ensure datumTypeColumn is in typesTable
  checkForMissingColumn(
    set$db$datumTypeColumn,
    set$db$columns$typesTableColumns,
    set$db$tables$typesTableName
  )

  # ensure batchNameColumnName is in batchesTable
  checkForMissingColumn(
    set$db$batchNameColumnName,
    set$db$columns$batchesTableColumns,
    set$db$tables$batchesTableName
  )

  #ensure typesPrimaryKey is in dataTable
  checkForMissingColumn(
    set$db$keys$typesPrimaryKey,
    set$db$columns$dataTableColumns,
    set$db$tables$dataTableName
  )

  #ensure batchesPrimayKey is in dataTable
  checkForMissingColumn(
    set$db$keys$batchesPrimaryKey,
    set$db$columns$dataTableColumns,
    set$db$tables$dataTableName
  )
}

# Utility function that converts the results of as.list(match.call()) back to a
# call object and evaluates the call.  Used to call .dpSet() from a argList
# generated inside dpSet() or loaded from disk by dpLoadSet()
.dpSetFromArgList = function(argList) {
  thisCall = do.call(call, c(list(".dpSet"), argList))
  newSet = eval(thisCall)
  return(newSet)
}

# Saving a dpSet object to disk directly runs the risk of the dpSet becomming
# out of date.  To avoid this problem, .dpSet() is an internal function called
# by dpSet() that generates a dpSet object from the arguments originally passed
# to dpSet(). dpSet() creates, executes, and saves a call to .dpSet() on disk,
# in the set's folder.  This saved call can be used to recreate a dpSet object
# from the same arguments in the future (i.e., "load the dpSet from disk")
# using: eval(callLoadedFromDisk). Thus, the resulting dpSet object will always
# be regenerated with the latest version of .dpSet(). Further, the arguments
# for the call saved on disk can be easily modified to add or remove arguments,
# if the arguments required by dpSet() change in the future.  Calls are s3
# objects built atop named lists, so code can load the call, modify the contents
# of the list, and resave the call to disk.
.dpSet = function(...) {

  # creates variables in this environment from the ... argument
  .unpackDots(...)

  connection = dpConnect(connectionArgs)

  newSet =
    list(
      setPath = setPath,
      connectionArgs = connectionArgs,
      db = list (
        tables = list(
          batchesTableName = batchesTableName,
          dataTableName = dataTableName,
          typesTableName = typesTableName
        ),
        columns = list(
          batchesTableColumns = RODBC::sqlColumns(connection, batchesTableName)$COLUMN_NAME,
          dataTableColumns = RODBC::sqlColumns(connection, dataTableName)$COLUMN_NAME,
          typesTableColumns = RODBC::sqlColumns(connection, typesTableName)$COLUMN_NAME
        ),
        keys = list(
          batchesPrimaryKey = RODBC::sqlPrimaryKeys(connection, batchesTableName)$COLUMN_NAME,
          dataPrimaryKey = RODBC::sqlPrimaryKeys(connection, dataTableName)$COLUMN_NAME,
          typesPrimaryKey = RODBC::sqlPrimaryKeys(connection, typesTableName)$COLUMN_NAME
        ),
        batchRowColumnName = batchRowColumnName,
        datumValueColumnName = datumValueColumnName,
        datumTypeColumnName = datumTypeColumnName,
        batchNameColumnName = batchNameColumnName
      )
    )
  RODBC::odbcClose(connection)
  class(newSet) = "dpSet"

  # Checks to be sure that tables and columns are consistent with database
  .dpValidateSet(newSet)

  return(newSet)

}

#' @rdname dpSet
is.dpSet = function(x) {
  return(is(x,"dpSet"))
}

