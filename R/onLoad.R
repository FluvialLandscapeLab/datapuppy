.onLoad <- function(libname, pkgname) {
  op <- options()
  op.loggerbase <- list(
    loggerbase.path = "~/R-dev",
    loggerbase.install.args = "",
    loggerbase.name = "Geoffrey Poole",
    loggerbase.desc.author = '"Geoffrey Poole <gpoole@montana.edu> [aut, cre]"',
    loggerbase.desc.license = "What license is it under?",
    loggerbase.desc.suggests = NULL,
    loggerbase.desc = list(),

    loggerbase.path = "basepath",
    loggerbase.files = "inputfiles",
    loggerbase.files.attr = list(name = "filename", impdef = "importdef"),
    loggerbase.files.tweaks = "corrections",
    loggerbase.impdefs = "importdefs",
    loggerbase.impdefs.attr = list(idx = "index", time = "datetime", fun = "rFunction"),
    loggerbase.root = "loggerbase",
    loggerbase.deps = "deployments",

    loggerbase.db.foreignKeys = "",
    loggerbase.db.database = "",
    loggerbase.db.sourceTable = "",
    loggerbase.db.dataTable = "",
    loggerbase.db.user = "",
    loggerbase.db.password = ""

#    loggerbase.xmlNodes = list(root = "loggerbase", path = "defaultpath", files = "files", importdefs = "importdefs"),
#    loggerbase.elementNames = list(files = "file", importdefs = "importdef", corrections = "correction"),
#    loggerbase.fileAttributes = list(filename = "filename", deployment = "deployment", importdefs = "importdefs", checksum = "checksum"),
#    loggerbase.importDefAttributes = list(idx = "idx", datetime = "datetime")
  )
  toset <- !(names(op.loggerbase) %in% names(op))
  if(any(toset)) options(op.loggerbase[toset])

  invisible()
}


