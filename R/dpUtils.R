dpConnect = function(connection, dataTableName, setTableName, metricTableName, metricFields) {

  fieldVars = c("sourceFields", "dataFields", "metricFields")

  requiredFields = list(
    getOption("datapuppy.db.reqSourceFields"),
    getOption("datapuppy.db.reqDataFields"),
    getOption("datapuppy.db.reqMetricFields")
  )

  test = as.list(match.call())[fieldVars]

  lacksRequiredFields = mapply(function(specified, required) any(!(required %in% names(eval(specified)))), test, requiredFields)
  if (any(lacksRequiredFields)) {
    stop("The following field lists lack required fields: ", paste0(fieldVars[lacksRequiredFields], collapse = '; '), "\n\n  Required Fields are: \n", paste0("    ", fieldVars, ": ", lapply(requiredFields, paste0, collapse = ", "), "\n"))
  }

  RODBC::sqlQuery(connect2DB, "SELECT deployment.* FROM deployment;")

  list(
    datapuppy.db.setTable = NULL,
    datapuppy.db.metricTable = NULL,
    datapuppy.db.dataTable = NULL,
    datapuppy.db.sourceRecord = NULL,
    datapuppy.db.dataRecord = NULL,
    datapuppy.db.metricRecord = NULL
  )

}


datapuppyConnect = function(userName, password) {
}
