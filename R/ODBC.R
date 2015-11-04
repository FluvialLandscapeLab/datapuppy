### DESIRED BEHAVIOR FOR LOADING DATA:
# Compare column names from dataframe to columns in data table.
# Those that don't match are assumed to be metrics.
# Those that do match are loaded along with anything from "foreignKeys" list.

tryit = function() {
# Must have DSN "meacham_heat" on computer
# To add a DSN for your DB, Control Pannel>Administrative Tools>Data Sources (ODBC)
# connect2DB <- odbcConnect("meacham_heat", uid="goff", pwd="taa2tosbt")

connect2DB <- RODBC::odbcDriverConnect("DSN=meacham_heat")
RODBC::sqlColumns(connect2DB, "importdata")$COLUMN_NAME
RODBC::sqlPrimaryKeys(connect2DB, "deployment")
RODBC::sqlTables(connect2DB)

# Query to return the deployment table
RODBC::sqlQuery(connect2DB, "SELECT * FROM importdata;")
}

dpColumnNames = function(connection, tableName) {
  RODBC::sqlColumns(connection, "deployment")$COLUMN_NAME
}

dpImport = function(node, importDefName) {

}
