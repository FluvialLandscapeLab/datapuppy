importHoboTempCond <- function(file){

  thisDataFrame <- read.csv(file)

  if (any(length(attributes(thisDataFrame)$names) != 5)) stop('The csv file you are trying to import does not have the correct number of columns')
  if (attributes(thisDataFrame)$names[3] != "Low.Range..?.S.cm..c.1.3" ) warning('The file you are trying to import may not be a Hobo Temperature/Conductivity File')

  attributes(thisDataFrame)$names[1] <- "Idx"
  attributes(thisDataFrame)$names[2] <- "DateTime"
  attributes(thisDataFrame)$names[3] <- "Conductivity"
  attributes(thisDataFrame)$names[4] <- "Temperature"
  attributes(thisDataFrame)$names[5] <- "SpecificConductance"

  return(data.frame(thisDataFrame))

}
