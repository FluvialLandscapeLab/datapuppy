ICFAdd = function(node, ICFTag, attrs = NULL, children = list(), parentElements = NULL, activate = TRUE, timeAdded = ICF.XMLTime()) {
  newElementName = ICFNextValue(node, ICFTag, parentElements)
  if(activate){
    added = ICF.XMLTime()
  } else {
    added = ""
  }
  attrs = c(attrs, added = added, removed = "")
  newElement = XML::xmlNode(newElementName, attrs = attrs, .children = children)
  tagPath = ICFTagPath(ICFTag, parentElements = parentElements)
  node = ICFUpdate(node, tagPath, newElement, replace = F)
  return(node)
}

ICFActiveChildren = function(node, ICFTag, parentElements = NULL, asOfTime = format(Sys.time(), usetz = T), asOfTimeZone = Sys.timezone()) {
  nodePart = ICFPart2(node, ICFTag, parentElements = parentElements)
  if(is.null(nodePart)) {
    stop("Request failed for '", ICFTag, "' from parent XML elements with ID number(s): ", paste(parentElements, collapse = ", "), ".  Check to be sure the element IDs are valid.")
  }
  children = XML::xmlChildren(nodePart)

  if(length(children) > 0) {
    asOfTime = as.POSIXct(asOfTime, tz = asOfTimeZone)
    asOfTime = ICF.POSIXct(ICF.XMLTime(asOfTime))

    added = sapply(children, function(x) XML::xmlAttrs(x)["added"])
    added = ICF.POSIXct(added)

    removed = sapply(children, function(x) XML::xmlAttrs(x)["removed"])
    removed[removed == ""] = ICF.XMLTime(asOfTime + 1)
    removed = ICF.POSIXct(removed)

    children = children[(asOfTime >= added) & ((asOfTime < removed))]
    class(children) = "XMLNodeList"
  }
  return(children)
}

ICFAttributeValues = function(node, ICFTag, attributeName, parentElements = NULL, asOfTime = format(Sys.time(), usetz = T), asOfTimeZone = Sys.timezone()) {

  activeChildren = ICFActiveChildren(node, ICFTag, parentElements, asOfTime, asOfTimeZone)

  if(length(activeChildren) == 0) {
    attrValues = character(0)
  } else {
    attrValues = sapply(activeChildren, function(x) XML::xmlAttrs(x)[attributeName], USE.NAMES = F)
    if(length(attrValues) == 0) {
      attrValues = character(0)
    } else {
      names(attrValues) = names(activeChildren)
    }
  }
  return(attrValues)
}


# ICFElements = function(node, elementType, parentElements = NULL, searchValue="*", attributeTag = NULL) {
#   middleOfXPath = paste0(ICFTagPath(elementType, parentElements), collapse = "/")
#   if(is.null(attributeTag)) {
#     xpath = paste0("/", ICFGetTag("root"), "/", middleOfXPath, "/", searchValue)
#   } else {
#     attributeName = ICFAttributeName(elementType, attributeTag)
#     xpath = paste0("/", ICFGetTag("root"), "/", middleOfXPath, "/*[@", attributeName, "='", searchValue, "']")
#   }
#   return(XML::getNodeSet(node, xpath))
# }

ICFElementIDs = function(node, ICFTag, parentElements = NULL, searchValue = NULL, attributeName = NULL, asOfTime = format(Sys.time(), usetz = T), asOfTimeZone = Sys.timezone()) {
  elements = ICFElements(node, ICFTag, parentElements, searchValue, attributeName, asOfTime, asOfTimeZone)
  return(sapply(elements, XML::xmlName))
}

ICFElements = function(node, ICFTag, parentElements = NULL, searchValue = NULL, attributeName = NULL, asOfTime = format(Sys.time(), usetz = T), asOfTimeZone = Sys.timezone()) {

  activeChildren = ICFActiveChildren(node, ICFTag, parentElements, asOfTime, asOfTimeZone)
  searchValue = as.character(searchValue)
  if(length(searchValue) > 0) {
    if(!is.null(attributeName)) {
      isRequested = (ICFAttributeValues(node, ICFTag, attributeName, parentElements, asOfTime, asOfTimeZone) == searchValue)

      if(!any(isRequested)) names(activeChildren) = NULL
    } else {
      isRequested = (sapply(activeChildren, XML::xmlName) == searchValue)
    }
    activeChildren = activeChildren[isRequested]
  }
  class(activeChildren) = "XMLNodeList"

  return(activeChildren)
}

ICFGetTag = function(tagVar) {
  return(getOption(paste0("datapuppy.", tagVar)))
}

ICFNextValue = function(node, ICFTag, parentElements = NULL) {
#  children = XML::xmlChildren(eval(ICFPart("node", ICFTag, parentElements)))
  children = XML::xmlChildren(ICFPart2(node, ICFTag, parentElements))
  if(length(children) > 0) {
    textValues = sapply(children, XML::xmlName)
    maxValue = as.character(max(as.numeric(textValues))+1)
  } else {
    maxValue = "1"
  }
  return(maxValue)
}

ICFReorderTweaks = function(node, datasetForeignKeyValue, newOrder = NULL, timeReordered = ICF.XMLTime()) {
  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)
  datasetID = ICFDatasetID(node, datasetForeignKeyValue)
  tweakOrders = ICFTweakOrders(node, datasetForeignKeyValue)
  if(is.null(newOrder)) {
    newOrder = as.character(order(tweakOrders))
    names(newOrder) = names(tweakOrders)
  }else{
    if(
      !identical(
        structure(sort(as.numeric(newOrder)), names = NULL),
        structure(sort(as.numeric(names(tweakOrders))), names = NULL)
      )
    ) {
      stop("Tweak order must be an list of active tweaks in the desired order.  For dataset with foreign key = ", datasetForeignKeyValue, ", active tweaks are: ", paste(names(tweakOrders), collapse = ", "))
    }
    newOrder = structure(as.character(1:length(newOrder)), names = as.character(newOrder))
    newOrder = newOrder[order(as.numeric(names(newOrder)))]
  }
  if(!identical(newOrder, tweakOrders)) {
    for (i in 1:length(tweakOrders)) {
      node = ICFRemove(
        node,
        ICFGetTag("orders"),
        tweakOrders[i],
        attributeName = ICFOrderAttributeNames()["ordervalue"],
        parentElements = c(datasetID, names(tweakOrders)[i]),
        timeRemoved = timeReordered
      )
      attributeVals = newOrder[i]
      names(attributeVals) = ICFOrderAttributeNames()["ordervalue"]
      node = ICFAdd(
        node,
        ICFGetTag("orders"),
        attrs = attributeVals,
        parentElements = c(datasetID, names(newOrder)[i]),
        timeAdded = timeReordered
      )
    }
  }
  return(node)
}

ICFDatasetID = function(node, datasetForeignKeyValue) {
  datasetID = ICFElementIDs(node, ICFGetTag("sets"), searchValue = datasetForeignKeyValue, attributeName = ICFDatasetAttributeNames()["datasetKey"])
  if(length(datasetID) != 1) {
    stop("There should be 1 dataset with a foreign key value of '", datasetForeignKeyValue, "', but ", length(datasetID), " were found.")
  }
  return(datasetID)
}

ICFGetElementFromPath = function(node, tagPath) {
  tpl = length(tagPath)
  if(tpl > 1) {
    element = ICFGetElementFromPath(node[[tagPath[1]]], tagPath[2:tpl])
  } else {
    element = node[[tagPath]]
  }
  return(element)
}

ICFPart2 = function(node, ICFTag, parentElements = NULL) {
  tagPath = ICFTagPath(ICFTag, parentElements)
  element = ICFGetElementFromPath(node, tagPath)
  return(element)
}

ICFPart = function(nodeName, ICFTag, parentElements = NULL, asText = F) {
  tagPath = ICFTagPath(ICFTag, parentElements)
  textExpression = paste0(nodeName, "[['", paste0(tagPath, collapse = "']][['"), "']]")
  #   if(!is.null(parentElements)) {
  #     testExpression = paste0(testExpression, "[['", parentElements, "']]")
  #   }
  if(asText) {
    return(textExpression)
  } else {
    return(parse(text = textExpression))
  }
}

ICF.POSIXct = function(XMLTime) {
  return(as.POSIXct(XMLTime, tz = "UTC"))
}

ICFRemove = function(node, ICFTag, searchValue, parentElements = NULL, attributeName = NULL, timeRemoved = ICF.XMLTime()) {
  elementList = ICFElements(node, ICFTag, parentElements = parentElements, searchValue = searchValue, attributeName = attributeName)
  if(length(elementList) == 0) {
    stop("There are no active elements with the requested searchValue in the specified node.  No element was removed from the node.")
  }
  element = elementList[[1]]
  attributes = XML::xmlAttrs(element)
  attributes["removed"] = timeRemoved
  XML::xmlAttrs(element) = attributes
  tagPath = ICFTagPath(ICFTag, parentElements)
  node = ICFUpdate(node, tagPath, element)
  for (child in XML::xmlChildren(element)) {
    activeSubChildren = ICFActiveChildren(node, XML::xmlName(child), c(parentElements, XML::xmlName(element)))
    for (subChild in activeSubChildren) {
      node = ICFRemove(node, XML::xmlName(child), XML::xmlName(subChild), c(parentElements, XML::xmlName(element)), timeRemoved = timeRemoved)
    }
  }
  return(node)
}

ICFTagsNamed = function() {
  xmlStructure = list(
    path = list(ICFGetTag("path")),
    impdefs = list(ICFGetTag("impdefs")),
    files = list(ICFGetTag("files")),
    sets = list(ICFGetTag("sets"),
                .keys = list(ICFGetTag("keys")),
                .tweaks = list(ICFGetTag("tweaks"),
                               .why = list(ICFGetTag("why")),
                               .orders = list(ICFGetTag("orders"))
                )
    )
  )
  return (unlist(xmlStructure))
}

ICFTagPath = function(ICFTag, parentElements = NULL) {
  if(!(ICFTag %in% ICFTagsNamed())) {
    stop("Element name '", ICFTag, "' is not valid.  Valid names are: \n   '", paste0(ICFTagsNamed(), collapse = ",' '"), "'")
  }
  tagPathNames = unlist(strsplit(names(ICFTagsNamed())[ICFTagsNamed() == ICFTag], ".", fixed = T))
  tagPath = lapply(tagPathNames, ICFGetTag)
  nullPathSegs = unlist(lapply(tagPath, is.null))
  if(!is.null(nullPathSegs)) {
    nullPathSegs = which(nullPathSegs)
  }
  if(length(nullPathSegs) != length(parentElements)) {
    stop("Parameter 'parentElements' has the wrong number of members for XML elements of type '", ICFTag, ".'")
  }
  for (i in 1:length(nullPathSegs)) tagPath[nullPathSegs[i]] = list(as.character(parentElements[i]))
  tagPath = unlist(tagPath)
  names(tagPath) = tagPathNames
  return(tagPath)
}

ICFTweakOrders = function(node, datasetForeignKeyValue) {
  datasetForeignKeyValue = ICFCheckForeignKeyValue(datasetForeignKeyValue)
  datasetID = ICFDatasetID(node, datasetForeignKeyValue)
  tweakList = ICFActiveChildren(node, ICFGetTag("tweaks"), parentElements = datasetID)
  if(length(tweakList)==0) {
    tweakOrders = integer(0)
  } else {
    tweakIDs = sapply(tweakList, XML::xmlName)
    tweakOrders = lapply(tweakIDs, function(x) ICFAttributeValues(node, ICFGetTag("orders"), ICFOrderAttributeNames()["ordervalue"], parentElements = c(datasetID, x)))
    tweakOrders = as.integer(unlist(tweakOrders))
    names(tweakOrders) = tweakIDs
  }
  return(tweakOrders)
}


ICFUpdate = function(node, tagPath, element, replace = T) {
  if(length(tagPath)>1) {
    node[[tagPath[1]]] = ICFUpdate(node[[tagPath[1]]], tagPath[2:length(tagPath)], element, replace = replace)
  } else {
    if(replace) {
      node[[tagPath]][[XML::xmlName(element)]] = element
    } else {
      node[[tagPath]] = XML::addChildren(node[[tagPath]], element)
    }
  }
  return(node)
}

ICF.XMLTime = function(targetTime = Sys.time()) {
  return(format(targetTime, tz="UTC", usetz = TRUE))
}
