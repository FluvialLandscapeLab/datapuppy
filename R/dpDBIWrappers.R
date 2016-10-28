
dpGetQuery = function(conn, statement, ...) {
  return(DBI::dbGetQuery(conn = conn, statement = statement, ...))
}

dpDisconnect = function(conn, ...) {
  return(DBI::dbDisconnect(conn, ...))
}

dpListTables = function(conn, ...) {
  return(DBI::dbListTables(conn, ...))
}

dpListFields = function(conn, name, ...) {
  return(DBI::dbListFields(conn, name, ...))
}

dpGetPrimaryKey = function(conn, name, ...) {
  if(class(conn) == "MySQLConnection") {
    pk = dpGetQuery(conn, paste0("SELECT COLUMN_NAME FROM information_schema.COLUMNS WHERE (TABLE_SCHEMA = '", DBI::dbGetInfo(conn)$dbname,"') AND (TABLE_NAME = '", name, "') AND (COLUMN_KEY = 'PRI');"))
  }
  return(pk$COLUMN_NAME)
}
