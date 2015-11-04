# ICFAddElement = function(node, ICFTagName, element) {
#   eval(ICFPart("node", ICFTagName)) = XML::addChildren(eval(ICFPart("node", ICFTagName)), element)
#   return(node)
# }

ICFAttributeName = function(elementType, attributeTag) {
  attributeNames = ICFGetTag(paste0(elementType, ".attr"))
  attributeName = attributeNames[[attributeTag]]
  if(is.null(attributeName)) {
    stop(paste0("Invalid attributeTag '", attributeTag, ".'  To designate attributes of ", ICFGetTag(elementType),", use: \n   '", paste0(names(attributeNames), "' to refer to ", attributeNames, collapse = ",\n   '")))
  }
  return(attributeName)
}

ICFCheckPath = function(path) {
  fullPath = normalizePath(path, "/", FALSE)
  if(file_test("-f", fullPath)) {
    stop("'", path, "' appears to refer to a file.  It must be a folder.")
  }
  if(!file_test("-d", fullPath)) {
    stop("'", path, "' is not a valid folder name.")
  }
  return(fullPath)
}

ICFCheckForeignKeyValue = function(keyValue) {
  datasetInteger = suppressWarnings(as.integer(keyValue))

  if(any(is.na(datasetInteger) | is.na(keyValue))) {
    stop("Foreign Key Value must be a positive, whole number.")
  }

  if(any((datasetInteger != keyValue) | (datasetInteger < 0)) | (length(datasetInteger) > 1) ) {
    stop("Foreign Key Value must be a positive, whole number.")
  }

  return(as.character(keyValue))
}

# ICFForeignKeys = function() {
#   keys = getOption("datapuppy.db.foreignKeys")
#   if(is.null(keys)) stop("The foreignKeys for the data table have not been set.  Use datapuppyConnect() to connect to your database.")
# }

ICFFileAttributeNames = function() {
  return(c(name = "file", impdef = "importdef", chksum = "checksum"))
}

ICFTweakAttributeNames = function() {
  return(c(command = "rCommand", why = "comment"))
}

ICFDatasetAttributeNames = function() {
  return(c(file = "importfile", datasetKey = "datasetforeignkey", firstLoadTime = "firstloaded", lastLoadTime = "lastloaded"))
}

ICFOrderAttributeNames = function() {
  return(c(ordervalue = "order"))
}

# ICFImpdefNames = function(node) {
#   names(node[[ICFGetTag("impdefs")]])
# }

ICFImpdefAttributeNames = function() {
  return(c(name = "name", fun = "rCommand"))
}

ICFPrimaryElementNames = function() {
    sectionTags = ICFPrimaryElementTags()
    sectionNames = lapply(sectionTags, ICFGetTag)
    names(sectionNames) = sectionTags
    return(sectionNames)
}

ICFPrimaryElementTags = function() {
  ## These data are not in options becuase they are immutable.  These tags are
  ## hardwired throughout the code.  You may add to this list, but do not change
  ## any existing values unless you search and replace the values throughout the
  ## code.
  return(c("path", "impdefs", "files", "sets"))
}

ICFDatasetElementTags = function() {
  ## These data are not in options becuase they are immutable.  These tags are
  ## hardwired throughout the code.  You may add to this list, but do not change
  ## any existing values.
  return(c("keys", "tweaks"))
}

ICFRootName = function() {
  return(getOption("datapuppy.root"))
}


# ICFImportDefParams = function(node, importDefName) {
#   importDef = ICFElements(node, ICFGetTag("impdefs"), importDefName)
#   if(length(importDef) == 0) {
#     stop(paste0("No importdef named '", importDefName, "' was found."))
#   }
#   if(length(importDef) > 1) {
#     stop(paste0("More than one importdef named '", importDefName), "' exists.")
#   }
#   importDefList = ICFElementsAsList(importDef[[1]])
#   importDefList[sapply(importDefList, "==", y = ")NA(")] = list(NA)
#   ## check for NULL last or error results
#   importDefList[sapply(importDefList, "==", y = ")NULL(")] = list(NULL)
#   return(importDefList)
# }

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
      stop("File '", filename, "' must exist, and must be located in '", basePath, "'.")
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
  XML::xmlValue(node[[ICFGetTag("path")]])
}

ICFUpdateElement = function(node, elementType, searchValue, newValue, attributeTag = NULL) {
  # get all of the nodes for the elementType
  defs = XML::xmlChildren(node[[ICFGetTag(elementType)]])
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
            attrs[ICFGetTag(paste0(elementType, ".attr"))[[attributeTag]] ] = newValue
            XML::xmlAttrs(td) = attrs
          }
          return(td)
        }
      )
    # replace the original elements with the updated elements
    defs[idx] = targetdefs
  }
  # within the root node, replace the elementType node with the updated elements
  node[[ICFGetTag(elementType)]] = XML::xmlNode(ICFGetTag(elementType), .children = defs)
  return(node)

}

