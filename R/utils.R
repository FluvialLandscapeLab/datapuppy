ICFAddElement = function(node, childName, element) {
  node[[childName]] = XML::addChildren(node[[childName]], element)
  return(node)
}

ICFAttributeName = function(elementType, attributeTag) {
  attributeNames = ICFTag(paste0(elementType, ".attr"))
  attributeName = attributeNames[[attributeTag]]
  if(is.null(attributeName)) {
    stop(paste0("Invalid attributeTag '", attributeTag, ".'  To designate attributes of ", ICFTag(elementType),", use: \n   '", paste0(names(attributeNames), "' to refer to ", attributeNames, collapse = ",\n   '")))
  }
  return(attributeName)
}

ICFForeignKeys = function() {
  keys = getOption("loggerbase.db.foreignKeys")
  if(is.null(keys)) stop("The foreignKeys for the data table have not been set.  Use datapuppyConnect() to connect to your database.")
}

ICFImpdefNames = function(node) {
  unlist(XML::xmlApply(node[[ICFTag("impdefs")]], XML::xmlName))
}
ICFImpdefAttrNames = function() {
  return(getOption("loggerbase.impdefs.attr"))
}

ICFFileAttributeNames = function() {
  return(getOption("loggerbase.files.attr"))
}

ICFPrimaryElementNames = function() {
    sectionTags = ICFPrimaryElementTags()
    sectionNames = lapply(sectionTags, ICFTag)
    names(sectionNames) = sectionTags
    return(sectionNames)
}

ICFPrimaryElementTags = function() {
  ## These data are not in options becuase they are immutable.  These tags are
  ## hardwired throughout the code.  You may add to this list, but do not change
  ## any existing values.
  return(list("path", "impdefs", "files", "deps"))
}

ICFRootName = function() {
  return(getOption("loggerbase.root"))
}

ICFTag = function(tagVar) {
  return(getOption(paste0("loggerbase.", tagVar)))
}

ICFElements = function(node, elementType, searchValue="*", attributeTag = NULL) {
  if(is.null(attributeTag)) {
    xpath = paste0("/", ICFTag("root"), "/", ICFTag(elementType), "/", searchValue)
  } else {
    attributeName = ICFAttributeName(elementType, attributeTag)
    xpath = paste0("/", ICFTag("root"), "/", ICFTag(elementType), "/*[@", attributeName, "='", searchValue, "']")
  }
  return(XML::getNodeSet(node, xpath))
}

ICFImportDefParams = function(node, importDefName) {
  importDef = ICFElements(node, "impdefs", importDefName)
  if(length(importDef) == 0) {
    stop(paste0("No importdef named '", importDefName, "' was found."))
  }
  if(length(importDef) > 1) {
    stop(paste0("More than one importdef named '", importDefName), "' exists.")
  }
  importDefList = ICFElementsAsList(importDef[[1]])
  importDefList[sapply(importDefList, "==", y = ")NA(")] = list(NA)
  ## check for NULL last or error results
  importDefList[sapply(importDefList, "==", y = ")NULL(")] = list(NULL)
  return(importDefList)
}

ICFElementsAsList = function(node) {
  children = XML::xmlChildren(node)
  values = lapply(children, XML::xmlValue)
  #if an empty string was stored in the XML, it comes back as character(0)
  #next line converts character(0) back to ""
  values[sapply(values, length) == 0] = ""
  names(values) = lapply(children, XML::xmlName)
  return(values)
}

ICFNormalizeFilename = function(node, filename, relative = T) {

  basePath = suppressWarnings(normalizePath(ICFPath(node), "/"))

  ## Preface filename with the basePath from the node.
  normalizedFilename = suppressWarnings(normalizePath(file.path(basePath, filename), "/"))

  ## If the filename doesn't exist, then it wasn't a valid filename relative to the base path.
  if(!file_test("-f", normalizedFilename)) {

    ## the following statement will yield a valid, fully specified file name if filename itself
    ## is fully specified, or if it is a valid filename relative to the R working directory.
    normalizedFilename = suppressWarnings(normalizePath(filename, "/"))

    #if the file doesn't exist or is not in the basePath, throw an error.
    if( (!file_test("-f", normalizedFilename)) || (substring(normalizedFilename, 1, nchar(basePath)) != basePath) ) {
      stop(paste0("Path ", filename, " doesn't exist in the base directory of the input control file or in the R working directory."))
    }
  }
  if(relative) {
    normalizedFilename = substring(normalizedFilename, nchar(basePath)+1)
    if(substring(normalizedFilename, 1, 1) == "/") {
      normalizedFilename = substring(normalizedFilename, 2)
    }
  }
  return(normalizedFilename)
}

ICFPath = function(node) {
  XML::xmlValue(node[[ICFTag("path")]])
}

ICFUpdateElement = function(node, elementType, searchValue, newValue, attributeTag = NULL) {
  # get all of the nodes for the elementType
  defs = ICFElements(node, elementType)
  if(length(defs) == 0) {
    stop(paste0("No ", elementType," found in XML node."))
  }

  # determing target nodes by value or attribute
  if(is.null(attributeTag)) {
    zap = sapply(defs, XML::xmlName) == searchValue
  } else {
    attributeName = ICFAttributeName(elementType, attributeTag)
    zap = sapply(defs, function(def) XML::xmlAttrs(def)[attributeName]) == searchValue
  }

  # trap if there are not target nodes
  if (!any(zap)){
    stop(paste0("No ", elementType, " associated with '", searchValue, "' was found.  Nothing updated."))
  }

  # Passing NULL in newValue means that the node should be deleted.  Retain the nodes that are not "zapped."
  # Otherwise, update the nodes with the new element value or attribute value.
  if(is.null(newValue)) {
    defs = defs[!zap]
  } else {
    # if the element value is to be updated, make sure the new value doesn't already exist.
    if(is.null(attributeTag)) {
      existing = sapply(defs, XML::xmlName) == newValue
      if(any(existing)){
        stop(paste0("A ", elementType, " named '", newValue, "' already exists.  Choose a different, unique name."))
      }
    }
    # update the values
    idx = which(zap)
    targetdefs =
      lapply(
        defs[idx],
        function(td) {
          if(is.null(attributeTag)) {
            XML::xmlName(td) = newValue
          } else {
            attrs = XML::xmlAttrs(td)
            attrs[ICFTag(paste0(elementType, ".attr"))[[attributeTag]] ] = newValue
            XML::xmlAttrs(td) = attrs
          }
          return(td)
        }
      )
    # replace the original elements with the updated elements
    defs[idx] = targetdefs
  }
  # within the root node, replace the elementType node with the updated elements
  node[[ICFTag(elementType)]] = XML::xmlNode(ICFTag(elementType), .children = defs)
  return(node)

}

