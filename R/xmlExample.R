###### BASE NODE ######################

#filename = "C:\\Users\\goff\\Documents\\R Projects\\loggerbase\\example2.xml"

#' Read an input control file for processing
#'
#' @param file Name of the import control file to read.
#' @return The contents of the import control file as an XMLNode object (see XML package documentation)
readICFNode = function(file) {
  doc = XML::xmlTreeParse(file)
  node = XML::xmlRoot(doc)
  # check to be sure the structure of the file corresponds to the expected input control node names.
if(XML::xmlName(node)!=ICFRootName() || !identical(sort(ICFPrimaryElementNames()), sort(names(XML::xmlChildren(node))))) {
    stop(paste(file, "may not be an input control file.  Names of nodes in the file don't match input control node names; run getOption('loggerbase.xmlNodes') to see expected names."))
  }
  return(node)
}

#' Create Import Control File as an XML node.
#'
#' @param path An optional root path that will be appended to datafile names in the input control file.
#' @return An XMLNode object (see XML package documentation) that represents the skeleton of an empty input control file.
createICFNode = function(path) {
  fullPath = normalizePath(path, "/", TRUE)
  if(!file_test("-d", fullPath)) {
    stop(paste(path, "is not a valid folder name.  It appears to refer to a file."))
  }

  ## Create the children nodes
  children = lapply(ICFPrimaryElementNames(), XML::xmlNode)

  ## Store the path in the path node
  children[["path"]] = XML::addChildren(children[["path"]], fullPath)

  ## Wrap the children in the root node and return the XML
  return(XML::xmlNode(ICFRootName(), .children = children))
}

###### DATAFILES ##################################
addDataFile = function(node, filename, importdef) {

  filename = ICFNormalizeFilename(node, filename)
  fullfilename = ICFNormalizeFilename(node, filename, relative = FALSE)

  names(filename) =  ICFFileAttributeNames()["name"]
  if(length(getDataFileSet(node, filename, "name")) > 0) {
    stop(paste0("'", fullfilename, "' is already defined as a datafile"))
  }
  if(!(importdef %in% ICFImpdefNames(node))) {
    stop(paste0("Requested importdef '", importdef, "' must be defined in the input control file before it can be used."))
  }

  if(!file.exists(fullfilename)) stop(paste("Requested file", fullfilename, "does not exist!"))
  checksum = tools::md5sum(fullfilename)
  attributeVector = c(filename, importdef)
  names(attributeVector) = ICFFileAttributeNames()
  newFile = XML::xmlNode(checksum, attrs = attributeVector)
  return(ICFAddElement(node, ICFTag("files"), newFile))
}

removeDataFile = function(node, filename) {
  return(ICFUpdateElement(node, "files", filename, NULL, "name"))
}

renameDataFile = function(node, filename, newFilename) {
  ####### NEED TO CHECK FOR EXISTENCE OF NEW NAME
  return(ICFUpdateElement(node, "files", filename, newFilename, "name"))
}

getDataFileSet = function(node, searchValue="*", attributeTag = NULL) {
  return(ICFElements(node, "files", searchValue, attributeTag))
}

###### COLUMN DEFINITIONS ##############################f
addImportDef = function(node, importDefName, idxColumnName, dateTimeColumnName, rImportCommand) {
  if(length(getImportDefSet(node, importDefName)) > 0) {
    stop(paste0("'", importDefName, "' is already defined as a importdef."))
  }
  attributeVector = c(idxColumnName, dateTimeColumnName, rImportCommand)
  names(attributeVector) = ICFImpdefAttrNames()
  impdef = XML::xmlNode(importDefName, attrs = attributeVector)
  return(ICFAddElement(node, ICFTag("impdefs"), impdef))
}

removeImportDef = function(node, importDefName) {
  ## CHECK TO BE SURE IMPORTDEF IS NOT USED BY A FILE!
  filesUsingImportDef = ICFElements(node, "files", importDefName, "impdef")
  if(length(filesUsingImportDef)>0) {
    stop(paste0("Can't remove importdef '", importDefName, "' because it is used by the following data file(s): \n", paste(lapply(filesUsingImportDef, function(x) XML::xmlAttrs(x)[ICFTag("files.attr")[["name"]] ]), collapse = ", \n"),"\n"))
  }
  return(ICFUpdateElement(node, "impdefs", importDefName, NULL))
}

renameImportDef = function(node, importDefName, newImportDefName) {
  node = ICFUpdateElement(node, "impdefs", importDefName, newImportDefName)
  ## UPDATE ALL FILES THAT USE THIS IMPORTDEF
  filesUsingImportDef = ICFElements(node, "files", importDefName, "impdef")
  if(length(filesUsingImportDef)>0) {
    node = ICFUpdateElement(node, "files", importDefName, newImportDefName, "impdef")
  }
  return(node)
}

getImportDefSet = function(node, importDefName="*", attributeTag = NULL) {
  return(ICFElements(node, "impdefs", importDefName, attributeTag))
}

### Deployments ######################
addDataSource = function(node, filename, foreignKeys) {

  filename = ICFNormalizeFilename(node, filename)
  fullfilename = ICFNormalizeFilename(node, filename, relative = FALSE)

  names(filename) =  ICFFileAttributeNames()["name"]
  if(length(getDataFileSet(node, filename, "name")) > 0) {
    stop(paste0("'", fullfilename, "' is already defined as a datafile"))
  }
  if(!(importdef %in% ICFImpdefNames(node))) {
    stop(paste0("Requested importdef '", importdef, "' must be defined in the input control file before it can be used."))
  }

  if(!file.exists(fullfilename)) stop(paste("Requested file", fullfilename, "does not exist!"))
  checksum = tools::md5sum(fullfilename)
  attributeVector = c(filename, importdef)
  names(attributeVector) = ICFFileAttributeNames()
  newFile = XML::xmlNode(checksum, attrs = attributeVector)
  return(ICFAddElement(node, ICFTag("files"), newFile))
}
