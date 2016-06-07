randomBatch = function(set) {
  batchRecord = structure(
    list(paste0(letters[sample(1:26, 10, replace = T)], collapse = "")),
    names = mySet$db$batchNameColumnName
  )
  return(dpBatch(batchRecord, set, makeRandomData(set)))
}

makeRandomData = function(set) {
  connection = dpConnect(set$connectionArgs)
  metricVector = RODBC::sqlQuery(
    connection,
    paste0("Select ", set$db$datumTypeColumnName, " FROM ", set$db$tables$typesTableName)
  )[ ,set$db$datumTypeColumnName]
  RODBC::odbcClose(connection)
  metricVector = levels(metricVector)
  metricVector = sample(metricVector, sample(c(1,2,3)))
  startDate = as.POSIXct(runif(1, max = as.integer(Sys.time())), origin = "1970-01-01")
  endDate = startDate + runif(1, min = 86400, max = 10*86400)
  dates = data.frame(seq(startDate, endDate, 1800))
  randomData = lapply(metricVector, function(x) runif(nrow(dates)) * sample(c(1,10,20,30), 1))
  return(
    structure(
      data.frame(
        do.call(cbind, c(list(dates), randomData))
      ),
      names = c("dateTime", metricVector)
    )
  )
}

randomTweaks = function(batch) {
  ## WHEN I UPDATE BATCHES IN MEMORY, I HAVE TO LOOK IN LISTS, TOO!  Maybe rapply?
}
