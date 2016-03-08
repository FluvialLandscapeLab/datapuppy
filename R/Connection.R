dpConnectionParams = function(odbcConnectFunctionName, parameters) {
  newConnectionParams =
    list(
      odbcConnectFunctionName = odbcConnectFunctionName,
      parameters = parameters
    )
  class(newConnectionParams) = c("dpConnectionParams")
  return(newConnectionParams)
}

dpConnect = function(dpConnectionParams) {
#  connFunction = get(dpConnectionParams$odbcConnectFunctionName, asNamespace("RODBC"))
#  odbcDriverConnect = get("odbcDriverConnect", asNamespace("RODBC"))
  connection = do.call(dpConnectionParams$odbcConnectFunctionName, args = c(dpConnectionParams$parameters, list(case = "nochange")))
  return(connection)
}
