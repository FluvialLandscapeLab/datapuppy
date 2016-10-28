#' Create a dpConnectionArgs object
#'
#' A dpConnectionArgs object is passed to the \code{dpSet()} function to create
#' a dpSet object. pdConnectionArgs objects specify the connect function (from
#' the DBI package) and the arguments passed to that function in order to create
#' to the set's database.
#' @param drv DBIDriver object, name, or DBIConnection.  See \code{drv} argument
#'   for \code{\link{dbConnect}} in the required \code{DBI} package.
#' @param ... authorization arguments needed by the DBMS instance; these
#'   typically include user, password, dbname, host, port, etc. For details see
#'   the appropriate \code{DBIDriver}.
#' @return A \code{dpConnectionArgs} object that can be passed to \code{dpSet()}
#'   or \code{dpConnect()}.
dpConnectionArgs = function(drv, ...) {
  arguments = eval(substitute(alist(...)))
  newConnectionArgs =
    lapply(
      c(
        list(drv = drv),
        arguments
      ),
      eval
    )
  class(newConnectionArgs) = c("dpConnectionArgs")
  return(newConnectionArgs)
}

#' Connect to a database using a dpConnectionArgs object.
#'
#' Open connections to ODBC databases using a \code{dpConnectionArgs} object
#'
#' @param dpConnectionArgs A \code{dpConnectionArgs} object created by the
#'   \code{\link{dpConnectionArgs}} function.
#' @return An DBI connection object (see \code{\link{dbConnect}} in the
#'   \code{DBI} package)
dpConnect = function(dpConnectionArgs) {
  ## might need case = "nochange" as a arguement to a MySQL connection
  connection = do.call(dbConnect, args = dpConnectionArgs)

  .dpValidConnection(connection)

  return(connection)
}
