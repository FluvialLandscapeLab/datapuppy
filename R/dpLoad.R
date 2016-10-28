#' Commit (upload) data from a dpBatch to the associated database.
#' @description Transfers the \code{data.frame} associated with a
#'   \code{\link{dpBatch}} object into the database described by
#'   \code{dpBatch}'s \code{\link{dpSet}}.  Any \code{\link{dpTweak}}s that are
#'   associated with the \code{dpBatch} are processed prior to loading the data
#'   into the database.
#' @param batch a \code{\link{dpBatch}} object.
#' @param fromDisk when TRUE, commits the most recent copy of the batch, which
#'   is loaded from the set folder stored on disk.  When FALSE, commits the
#'   batch as it is passed to dpCommitBatchData. Because tweaks can be saved to
#'   disk, but not reflected in a batch object resident in memory.  "fromDisk =
#'   TRUE" as a default is intended to ensure that any tweaks that were saved to
#'   the batch on disk are included in the commit.
dpCommitBatchData = function(batch, fromDisk = TRUE) {
  if(!is.dpBatch(batch)) batch = dpLoadBatch(batch)

  set = dpLoadSet(batch$setPath)

  if(fromDisk) {
    batch = dpLoadBatch(set, batch$batchName)
  }

  batch$batchData = dpProcessTweaks(batch)

  .dpValidateData(batch$batchData, set)

  connection = dpConnect(set$connectionArgs)

  #get the names of valid data types in the types table and their indexes
  typeData = dpGetQuery(connection, paste0("SELECT * FROM ", set$db$tables$typesTableName))
  typeNames = as.character(typeData[,set$db$datumTypeColumnName])
  typeIDX = structure(
    typeData[,set$db$keys$typesPrimaryKey],
    names = typeNames
  )

  # any column in batch$batchData where the name is in typeNames is a column containing data
  dataColumnNames = colnames(batch$batchData)[colnames(batch$batchData) %in% typeNames]

  # add the datumIDX, batchIDX, and batchRow to the batch$batchData
  if(length(set$db$keys$dataPrimaryKey) == 0) {
    calculatedFieldParamList =
      structure (
        list(batch$batchIDX),
        names = c(set$db$keys$batchesPrimaryKey)
      )
  } else {
    calculatedFieldParamList =
      structure (
        list(0, batch$batchIDX),
        names = c(set$db$keys$dataPrimaryKey, set$db$keys$batchesPrimaryKey)
      )
  }
  batch$batchData = cbind(do.call(data.frame, calculatedFieldParamList),  batch$batchData)

  # make a list of names of columns that are NOT data columns
  baseColumnNames = colnames(batch$batchData)[!(colnames(batch$batchData) %in% dataColumnNames)]

  # the lapply, below yields a list of dataframes -- one data frame for each data column
  # each dataframe has all of the non-data columns, but one data column.  rbind() is then
  # applied to the dataframes in the list to create a data suitable for loading into the
  # datatable.
  batch$batchData = do.call(
    rbind,
    lapply(
      dataColumnNames,
      function(cName) {
        structure(
          cbind(batch$batchData[c(baseColumnNames, cName)], as.integer(typeIDX[cName])),
          names = c(baseColumnNames, set$db$datumValueColumnName, set$db$keys$typesPrimaryKey)
        )
      }
    )
  )

  if(!(identical(sort(names(batch$batchData)), sort(set$db$columns$dataTableColumns)))) {
    stop("ERROR: The transformed batchData table has columns named:",
         paste(names(batch$batchData), collapse = ", "),
         "\n  Required names are the columns in the dataTable: ",
         paste(set$db$columns$dataTableColumns, collapse = ", ")
    )
  }

  batch$batchData = .dpDateColToCharacter(batch$batchData)

  dpDisconnect(connection)

  .dpTransferBatch(batch)

  save(batch, file = file.path(batch$batchPath, "committed", paste0(gsub(":", "_", UTCTime()), ".rData")))
}
