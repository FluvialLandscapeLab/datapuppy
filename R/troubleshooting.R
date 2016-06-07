#' dpSet and dpBatch troubleshooting fuctions
#'
#' These functions allow investigation and manipulation of arguments stored on
#' disk.  Stored arguements are used to re-load \code{dpSet} and \code{dpBatch}
#' S3 objects into memory. Under normal use, these fuctions are not necessary;
#' they were created during development of \code{datapuppy} for troubleshooting
#' and fixing corrupted \code{dpSets} and \code{dpBatchs}. These functions might
#' be useful for advanced \code{datapuppy} users.  Use with caution.  Before
#' using these functions, fully understand: 1) the structure of the S3 objects;
#' and 2) how the arguements stored on disk are used to recreate the objects are
#' stored on disk.
#'
#' @details \code{dpValidateSetArgNames()} (\code{dpValidateBatchArgNames})
#'   compares the names of the arguments stored on disk in the
#'   \code{dpSet.rData} (\code{dpBatch.rData}) file to the names of
#'   \code{\link{formals}(dpSet)} (\code{\link{formals}(dpBatch)}) to ensure the
#'   stored argument names are compatible with the constructor functions.
#'
#' @param set A \code{\link{dpSet}} object describing the \code{set} to be
#'   operated on. Alternatively, a setPath (see 'setPath' argument in
#'   \code{\link{dpSet}}) from which the \code{dpSet} will be loaded.
#' @param reportError Boolean value when set to \code{TRUE} creates a printed
#'   message and stops execution if the validation fails.  Set to \code{FALSE},
#'   any errors are returned in a list (see code{Value} section, below.)
#' @param batch A \code{\link{dpBatch}} object describing the \code{batch} to be
#'   operated on.  Alternatively, a \code{batchPath} from which the
#'   \code{dpBatch} will be loaded.
#' @param setPath A character string containing the path to the \code{set}
#'   directory.  See \code{\link{dpSet}} for more details. If a path is not
#'   fully specified, it is assumed to be a subdirectory of the R working
#'   directory (see \code{\link{getwd}}).
#' @param validate A boolean determining whether names in the argument list are
#'   validated against \code{\link{formals}(dpSet)} or
#'   \code{\link{formals}(dpBatch)}.  If TRUE, an error is thrown and execution
#'   stops when the names of the ArgList don't match.
#' @param batchPath A character string containing the path to the \code{batch}
#'   directory.  See \code{\link{dpBatch}} for more details. If a path is not
#'   fully specified, it is assumed to be a subdirectory of the R working
#'   directory (see \code{\link{getwd}}).  If NULL, the batchPath is constructed
#'   from the values expected in the argList.
#' @param argList A list of arguments used to create a \code{dpSet} or
#'   \code{dpBatch}.  The list is usually derived from \code{dpLoadSetArgList()}
#'   or \code{dpLoadBatchArgList()}.
#' @return \code{dpValidateSetArgNames()} (\code{dpValidateBatchArgNames})
#'   returns a list containing two character vectors. The first vector, named
#'   "missingArguments", contains the names of arguments required by
#'   \code{dpSet()} (\code{dpBatch()}) that are missing from the stored
#'   arguments.  The second vector, named "extraArguments" contains the names of
#'   any extra arguments in the stored arguments, which should be removed or
#'   renamed. When \code{reportError} is TRUE, the same information is thrown as
#'   an error and execution is stopped.
#' @export
dpValidateSetArgNames = function(set, reportError = TRUE) {
  if(class(set) == "dpSet") set = set$setPath
  #Load the call list without validating so we get the call list even if there
  #is an error in it
  argList = dpLoadSetArgList(set, validate = FALSE)
  #Validate the list.  Return the results of the validation to the user.
  return(.dpValidateArgListNames(argList, "dpSet", reportError))
}

#' @rdname dpValidateSetArgNames
dpValidateBatchArgNames = function(batch, reportError = TRUE) {
  if(is.dpBatch(batch)) batch = batch$batchPath
  #Load the call list without validating so we get the call list even if there
  #is an error in it
  argList = dpLoadBatchArgList(batch, validate = FALSE)

  #Validate the list.  Return the results of the validation to the user.
  return(.dpValidateArgListNames(argList, "dpBatch", reportError))
}


#' @rdname dpValidateSetArgNames
#' @details \code{dpLoadSetArgList} (\code{dpLoadBatchArgList}) allow the user to
#' investigate the \code{list} of stored arguments.  \code{dpSaveSetArgList}
#' (\code{dpSaveBatchArgList}) save loaded argument \code{lists} back to
#' disk, overwriting the original arguments unless a new path is specified in
#' the argument list. Ideally, these fuctions should not be required by the end
#' user, but they are provided for troubleshooting purposes or for altering the
#' attributes of an existing \code{set} (\code{batch}).  Use these functions
#' with care -- it would be easy to corrupt a \code{set} (\code{batch}) if you
#' don't really understand what you are doing.
#' @return \code{dpLoadSetArgList()} returns the list of arguments that was
#'   originally passed to \code{dpSet()} to create the set. By default, the list
#'   will be validated against \code{formals(dpSet)} and an error will be thrown
#'   if the argument list doesn't match.  If validate=FALSE, the function will
#'   return the loaded argument list, so that an invalid call list can be
#'   inspected or repaired.
dpLoadSetArgList = function(setPath, validate = TRUE) {
  argList = .dpLoadArgList(setPath, "dpSet", validate)
  # put the current setPath in the argList in case the folder has been moved.
  if(validate) argList[["setPath"]] = setPath
  return(argList)
}

#' @rdname dpValidateSetArgNames
dpSaveSetArgList = function(argList, validate = TRUE) {
  return(.dpSaveArgList(argList, argList$setPath, "dpSet", validate))
}

#' @rdname dpValidateSetArgNames
dpLoadBatchArgList = function(batchPath, validate = TRUE) {
  argList = .dpLoadArgList(batchPath, "dpBatch", validate)
  # Strip off the last folder of the batch path then set resulting path to the
  # "set" value in the argList.  This ensures that set path is current.
  if(validate) {
    pathParts = strsplit(batchPath, "/", fixed = T)[[1]]
    pathParts = pathParts[1:(length(pathParts)-1)]
    argList$set = do.call(file.path, as.list(pathParts))
  }
  return(argList)
}

#' @rdname dpValidateSetArgNames
dpSaveBatchArgList = function(argList, batchPath = NULL, validate = TRUE) {

  # .dpSaveArgList will validate the argList as well, but before
  # .dpSaveArgList is called, code relies on argList$set and
  # argList$batchRecord.  So validate the argList first for a cleaner error
  # message if set or batchRecord are missing from argList.
  if(validate) {
    .dpValidateArgListNames(argList, "dpBatch", reportError = TRUE)
  }

  if(!is.dpSet(argList$set)) argList$set = dpLoadSet(argList$set)
  # create a batchPath if one is not provided
  if(is.null(batchPath)) {
    batchPath = .batchPath(argList$set, argList$batchRecord[argList$set$db$batchNameColumnName])
  }
#   # put the setPath in the arglist prior to storing.
#   if(validate) {
#     if(is.dpSet(argList$set)) {
#       argList$set = set$setPath
#     }
#   }
  .dpSaveArgList(argList, batchPath, "dpBatch", validate)
}


