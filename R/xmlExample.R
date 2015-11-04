####### BASE NODE ######################

#filename = "C:\\Users\\goff\\Documents\\R Projects\\datapuppy\\example2.xml"

#' Read an input control file for processing
#'
#' @param file Name of the import control file to read.
#' @return The contents of the import control file as an XMLNode object (see XML package documentation)
#' @export
readICFNode = function(file) {
  doc = XML::xmlTreeParse(file)
  node = XML::xmlRoot(doc)
  # check to be sure the structure of the file corresponds to the expected input control node names.
  if(XML::xmlName(node)!=ICFRootName() || !identical(sort(ICFPrimaryElementNames()), sort(names(XML::xmlChildren(node))))) {
    stop("'", file, "' might not be an input control file.  Names of nodes in the file don't match input control node names.  Expected element names (which can be changed by setting datapuppy options with setOptions() function) are: \n  ", paste0(ICFPrimaryElementNames(), collapse = "\n  "))
  }
  return(node)
}

#' Create Import Control File as an XML node.
#'
#' @param path An optional root path that will be appended to datafile names in the input control file.
#' @return An XMLNode object (see XML package documentation) that represents the skeleton of an empty input control file.
#' @export
createICFNode = function(path) {

  fullPath = ICFCheckPath(path)

  ## Create the children nodes
  children = lapply(ICFPrimaryElementNames(), XML::xmlNode)

  ## Store the path in the path node
  children[["path"]] = XML::addChildren(children[["path"]], fullPath)

  ## Wrap the children in the root node and return the XML
  return(XML::xmlNode(ICFRootName(), .children = children))
}

#' @export
updatePath = function(node, path) {
  ICFCheckPath(path)
  pathElement = node[[ICFGetTag("path")]]
  XML::xmlValue(pathElement) = path
  pathPath = ICFTagPath("path")
  node = ICFUpdate(node, pathPath, pathElement)
  return(node)
}

###### DATAFILES ##################################

#' @export
addDatafile = function(node, filename, importdef) {

  filename = ICFNormalizeFilename(node, filename)
  fullfilename = ICFNormalizeFilename(node, filename, relative = FALSE)

  names(filename) = ICFFileAttributeNames()["name"]
  if(filename %in% ICFAttributeValues(node, ICFGetTag("files"), ICFFileAttributeNames()["name"])) {
    stop(paste0("'", fullfilename, "' is already defined as a datafile"))
  }
  if(!(importdef %in% ICFAttributeValues(node, ICFGetTag("impdefs"), ICFImpdefAttributeNames()["name"]))) {
    stop(paste0("Requested importdef '", importdef, "' must be defined in the input control file before it can be used."))
  }

  checksum = tools::md5sum(fullfilename)
  attributeVector = c(filename, importdef, checksum)
  names(attributeVector) = ICFFileAttributeNames()
  return(ICFAdd(node, ICFGetTag("files"), attributeVector))
}

#' @export
removeDatafile = function(node, file) {
  file = as.character(file)
  return(ICFRemove(node, ICFGetTag("files"), file, attributeName = ICFFileAttributeNames()["name"]))

}

getDatafileSet = function(node, searchValue = NULL, attributeName = NULL) {
   return(ICFElements(node, ICFGetTag("files"), searchValue = searchValue, attributeName = attributeName))
}

###### COLUMN DEFINITIONS ##############################f

#' @export
addImportDef = function(node, impdefName, rImportCommand) {
  if(impdefName %in% ICFAttributeValues(node, ICFGetTag("impdefs"), ICFImpdefAttributeNames()["name"])) {
    stop(paste0("An importdef by the name of '", impdefName, "' has already been defined."))
  }
  attributeVector = c(impdefName, rImportCommand)
  names(attributeVector) = ICFImpdefAttributeNames()
  return(ICFAdd(node, ICFGetTag("impdefs"), attributeVector))
}

#' @export
removeImportDef = function(node, impdefName) {
  impdefName = as.character(impdefName)
    ## CHECK TO BE SURE IMPORTDEF IS NOT USED BY A FILE!
  filesUsingImportDef = (impdefName %in% ICFAttributeValues(node, ICFGetTag("files"), ICFFileAttributeNames()["impdef"]))
  if(filesUsingImportDef) {
    fileNamesUsingImportDef = sapply(ICFElements(node, ICFGetTag("files"), searchValue = impdefName, attributeName = ICFFileAttributeNames()["impdef"]), function(x) XML::xmlAttrs(x)[ICFFileAttributeNames()["name"]])
    stop(paste0("Can't remove importdef '", impdefName, "' because it is used by the following data file(s):\n    ", paste(fileNamesUsingImportDef, collapse = ",\n    "),"\n"))
  }
  return(ICFRemove(node, ICFGetTag("impdefs"), impdefName, ICFImpdefAttributeNames()["name"]))
}

getImportDefSet = function(node, searchValue = NULL, attributeName = NULL) {
  return(ICFElements(node, ICFGetTag("impdefs"), searchValue = searchValue, attributeName = attributeName))
}

### Data Sets ######################
addDataset = function(node, datasetForeignKeyValue, datafileName, foreignKeys = NULL) {

  datafileNames = ICFAttributeValues(node, ICFGetTag("files"), ICFFileAttributeNames()["name"])
  if(!(datafileName %in% datafileNames)) {
    stop("'", datafileName, "' is not defined as a datafile.")
  }

  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)

  datasetForeignKeyValues = ICFAttributeValues(node, ICFGetTag("sets"), ICFDatasetAttributeNames()["datasetKey"])
  if(datasetForeignKeyValue %in% datasetForeignKeyValues) {
    stop("datasetForeignKeyValue must be unique, but is already defined.")
  }

  attributeVector = c(datafileName, datasetForeignKeyValue, "", "")
  names(attributeVector) = ICFDatasetAttributeNames()
  if(!is.null(foreignKeys)) {
    if(is.null(names(foreignKeys)) || class(foreignKeys) != "numeric") {
      stop("'foreignKeys' must be a *named* vector, of type *numeric*")
    }
    if(any(nchar(names(foreignKeys)) == 0)) {
      stop("All values in 'foreignKeys' must have names.")
    }
    foreignKeys = unlist(lapply(foreignKeys, ICFCheckForeignKeyValue))
#     keyChildren = mapply(XML::xmlNode, names(foreignKeys), foreignKeys, SIMPLIFY = F)
#     XML::xmlChildren(foreignKeyNode) = keyChildren
  }
  foreignKeyNode = XML::xmlNode(ICFGetTag("keys"), attrs = foreignKeys)
  tweakNode = XML::xmlNode(ICFGetTag("tweaks"))
#  tweakOrdersNode = XML::xmlNode(ICFGetTag("tweakorders"))
  return(ICFAdd(node, ICFGetTag("sets"), attributeVector, list(foreignKeyNode, tweakNode)))

}

removeDataset = function(node, datasetForeignKeyValue) {
  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)
  return(ICFRemove(node, ICFGetTag("sets"), datasetForeignKeyValue, attributeName = ICFDatasetAttributeNames()["datasetKey"]))
}

# rekeyDataset = function(node, datasetForeignKeyValue, newdatasetForeignKeyValue) {
#     node = ICFUpdateElement(node, "sets", datasetForeignKeyValue, newdatasetForeignKeyValue)
#     return(node)
# }

addTweak = function(node, datasetForeignKeyValue, rCommand, why) {
  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)
  datasetID = ICFDatasetID(node, datasetForeignKeyValue)
  rCommands = ICFAttributeValues(node, ICFGetTag("tweaks"), ICFTweakAttributeNames()["command"], parentElements = datasetID)
  if(rCommand %in% rCommands) {
    stop(paste0("The command '", rCommand, "' is already defined as a tweak for the requested dataset."))
  }
  tweakOrderNode = XML::xmlNode(ICFGetTag("orders"))
  attributeVector = c(rCommand, why)
  names(attributeVector) = ICFTweakAttributeNames()
  tweakID = ICFNextValue(node, ICFGetTag("tweaks"), parentElements = datasetID)
  tweakOrders = ICFTweakOrders(node, datasetForeignKeyValue)
  if(length(tweakOrders) == 0) {
    nextTweakOrder = 1
  } else {
    nextTweakOrder = max(as.numeric(tweakOrders)) + 1
  }
  timeAdded = ICF.XMLTime()
  node = ICFAdd(node, ICFGetTag("tweaks"), attributeVector, list(tweakOrderNode), parentElements = datasetID, timeAdded = timeAdded)
  attributeVector = nextTweakOrder
  names(attributeVector) = ICFOrderAttributeNames()
  node = ICFAdd(node, ICFGetTag("orders"), attrs = attributeVector, parentElements = c(datasetID, tweakID), timeAdded = timeAdded)
  return(node)
}

removeTweak = function(node, datasetForeignKeyValue, rCommand) {
  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)
  datasetID = ICFDatasetID(node, datasetForeignKeyValue)
  timeOfAction = ICF.XMLTime()
  node = ICFRemove(node, ICFGetTag("tweaks"), rCommand, attributeName = ICFTweakAttributeNames()["command"], parentElements = datasetID, timeRemoved = timeOfAction)
  node = ICFReorderTweaks(node, datasetForeignKeyValue)
  return(node)
}
