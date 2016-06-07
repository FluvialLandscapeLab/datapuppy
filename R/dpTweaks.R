dpTweak = function(batch, fun, args, reason, after = length(batch$tweaks), updateGlobalEnv = TRUE) {

 ##### STORE THE FUNCTION IN THE TWEAK!!!!  EVEN IF THE FUNCTION CHANGES IN THE
 ##### FUTURE, THE OLD FUNCTION WILL BE CALLED!!!!!  SO COOL!!!!  Wait...  If
 ##### the function calls something else that changes, then there isn't any
 ##### guarentee that the data will stay the same....  DAMN.

 ## Hmmm...  Here is an idea...  What if you save just the "values" field each time the dataset are loaded...  That's brilliant!

  if(class(batch) != "dpBatch") {
    stop("'batch' argument must be a dpBatch object.  Use dpBatch() or dpLoadBatch() to create a dpBatch object.")
  }

  # Error checking and set up...
#   if(!is.character(batchName)) {
#     stop("'batchName' must be a character string.")
#   }
#
#   if(!is.dpSet(set)) set = dpLoadSet(set)
#   batch = dpLoadBatch(set, batchName)

  if(class(fun) != "function") {
    if(class(fun) != "character") {
      stop("'fun' must be a function or a character string naming a function.")
    }
    if(!exists(fun)) {
      stop("Can't find the function named '", fun, "'.")
    }
    fun = get(fun)
  }

  if (!is.list(args)) stop("'args' argument must be a named list.")

  if(after != as.integer(after)) {
    stop("'after' argument must be a whole number.")
  }

  funFormals = formals(args(fun))
  allowableArgNames = names(funFormals)
  if(allowableArgNames[[1]] != "x") {
    stop("The first argument for the requested function must be named 'x'.")
  }
  requiredArgNames = names(.formalsWithoutDefaults(funFormals))
  missingArgNames = requiredArgNames[!(requiredArgNames %in% c(names(args), "x"))]
  if(length(missingArgNames)>0) {
    stop("The requested function requires the following arguments.  Thus the 'args' list must have the following names: ", paste0(missingArgNames, collapse = ", "))
  }
  illegalNames = names(args)[!names(args) %in% allowableArgNames]
  if(length(illegalNames) > 0) {
    stop("The requested function doesn't allow the following arguments; thus the following names can not be in the 'args' list: ", paste0(illegalNames, collapse = ", "))
  }
  if("x" %in% names(args)) {
    stop("Do not include the 'x' argument in the args list.  Datapuppy will automatically pass the batch data to the function as the 'x' argument.")
  }

  newTweak =
    structure(
      list(
        fun = as.character(as.list(match.call())$fun),
        args = args,
        reason = reason,
        created = UTCTime()
      ),
      class = "dpTweak"
    )

  batch$tweaks = append(batch$tweaks, list(newTweak), after)
  tweakList = batch$tweaks
  save("tweakList", file = file.path(batch$batchPath, "dpTweaks.rData"))
  print("New tweak list is:")
  print(batch$tweaks)

  # search global environment to see if copies of the batch are stored in
  # memory.  If so, update the tweak lists.
  if (updateGlobalEnv) {
    batches = objects(envir = .GlobalEnv)
    batches = batches[sapply(batches, function(x) is.dpBatch(get(x, envir = .GlobalEnv)))]
    for (batchName in batches) {
      if(get(batchName, envir = .GlobalEnv)$setPath == batch$setPath && get(batchName, envir = .GlobalEnv)$batchIDX == batch$batchIDX) {
        assign(batchName, batch, envir = .GlobalEnv)
      }
    }
  }
  invisible(batch)
}

print.dpTweak = function(x, ...) {
  quotedArgs =
    lapply(
      x$args,
      function(x) {
        if(is.character(x)) {
          x = paste0("'", x, "'")
        }
        return(x)
      }
    )
  writeLines(
    paste0(
      "<", format(as.POSIXct(x$created, tz = "UTC"), tz = Sys.timezone(), usetz = T), "> ", x$reason, "\n  ",
      x$fun,
      "(x = <batchData>, ",
      paste0(names(x$args), " = ", quotedArgs, collapse = ", "),
      ")"
    )
  )
}

dpProcessTweaks = function(batch) {
  for (tweak in batch$tweaks) {
    batch = do.call(tweak$fun, c(list(x = batch), tweak$args))
  }
  return(batch$batchData)
}

dpTweakAddConstant = function(x, columnName, constant, index = TRUE) {
  x$batchData[,columnName] = x$batchData[,columnName] + constant
  return(x)
}

dpTweakCalibrate = function(x, columnName, slope, intercept, index = TRUE) {
  x$batchData[,columnName] = slope * x$batchData[,columnName] + intercept
  return(x)
}

dpTweakDeleteRows = function(x, batchRows, index = TRUE) {
  set = dpLoadSet(x$setPath)
  x$batchData = x$batchData[!(x$batchData[,set$db$batchRowColumnName] %in% batchRows),]
  return(x)
}

dpTweakMarkAsNA = function(x, batchRows, columnName, index = TRUE) {
  if(!is.dpSet(x$set)) x$set = dpLoadSet(x$set)
  x$batchData[x$batchData[,x$set$db$batchRowColumnName] %in% batchRows, columnName] = NA
  return(x)
}

#
# fn=function(x) {
#   x+1 # A comment, kept as part of source
# }
#
# fileConn<-file("test.R")
# writeLines(c("df <- data.frame(v1=c(1, 1, 1), v2=c(1, 2, 3))",
#              "\n",
#              "m <- mean(df$v2)",
#              "\n",
#              "describe(df)  #psych package"),
#            fileConn)
# close(fileConn)
#
# # getParseData approach
# pkg <- getParseData(parse(file.choose()))
# pkg <- pkg[pkg$token=="SYMBOL_FUNCTION_CALL",]
# pkg <- pkg[!duplicated(pkg$text),]
# pkgname <- pkg$text
# pkgname
# # [1] "data.frame" "c"          "mean"       "describe"
#
# # load all probable packages first
# pkgList <- list(pkgname)
# for (i in 1:length(pkgname)) {
#   try(print(packageName(environment(get(pkgList[[1]][i])))))
# }
#
# #[1] "base"
# #Error in packageName(environment(get(pkgList[[1]][i]))) :
# #  'env' must be an environment
# #[1] "base"
# #[1] "psych"
