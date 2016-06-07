#' Create a dpConnectionArgs object
#'
#' A dpConnectionArgs object is passed to the \code{dpSet()} function to
#' create a dpSet object. pdConnectionArgs objects specify the connect
#' function (from the RODBC package) and the arguments passed to that function
#' in order to create to the set's database.
#' @param odbcConnectFunctionName A \code{character} variable specifying the
#'   function (from the RODBC package) used to connect to a database.
#' @param arguments  A named \code{list} of arguments passed to
#'   \code{odbcConnectFunction} where the names in the \code{list} are the names
#'   of the function arguments.
#' @return A \code{dpConnectionArgs} object that can be passed to
#'   \code{dpSet()} or \code{dpConnect()}.
dpConnectionArgs = function(odbcConnectFunctionName, arguments) {
  newConnectionArgs =
    list(
      odbcConnectFunctionName = odbcConnectFunctionName,
      arguments = arguments
    )
  class(newConnectionArgs) = c("dpConnectionArgs")
  return(newConnectionArgs)
}

#' Connect to a database using a dpConnectionArgs object.
#'
#' Open connections to ODBC databases using a \code{dpConnectionArgs} object
#'
#' @param dpConnectionArgs A \code{dpConnectionArgs} object created by the
#'   \code{\link{dpConnectionArgs()}} function.
#' @return An RODBC connection object (see \code{\link{odbcConnect}} in the
#'   \code{RODBC} package)
dpConnect = function(dpConnectionArgs) {
  connection = do.call(dpConnectionArgs$odbcConnectFunctionName, args = c(dpConnectionArgs$arguments, list(case = "nochange")))
  return(connection)
}
