#' @importClassesFrom XML XMLNode
#' @importFrom RODBC odbcDriverConnect

NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.datapuppy <- list(
    datapuppy.path = "~/R-dev",
    datapuppy.install.args = "",
    datapuppy.name = "Geoffrey Poole",
    datapuppy.desc.author = '"Geoffrey Poole <gpoole@montana.edu> [aut, cre]"',
    datapuppy.desc.license = "What license is it under?",
    datapuppy.desc.suggests = NULL,
    datapuppy.desc = list(),

    datapuppy.root = "datapuppy",
    datapuppy.tweaks = "tweaks",
    datapuppy.keys = "foreignkeys",
    datapuppy.tweakorders = "tweakorders",
    datapuppy.why = "comment",
    datapuppy.orders = "ordering",

    datapuppy.path = "basepath",
    datapuppy.files = "datafiles",
#    datapuppy.files.attr = list(impdef = "importdef", chksum = "checksum"),
    datapuppy.impdefs = "importdefs",
#    datapuppy.impdefs.attr = list(fun = "rCommand"),
    datapuppy.sets = "datasets",
#    datapuppy.sets.attr = list(file = "importfile"),
    datapuppy.sets.keys = "foreignkeys",
    datapuppy.sets.tweaks = "tweaks",
    datapuppy.sets.tweakorders = "tweakorders",
    datapuppy.sets.tweaks.attr = list(activate = "added", deactivate = "removed"),

    datapuppy.db.reqObservationFields = c(observationID = "ObservationIDX",
                                          observationDateTime = "ObservationTime",
                                          datasetID = "DeploymentIDX",
                                          metricID = "MetricIDX",
                                          value = "Value"
                                          ),
    datapuppy.db.reqDatasetFields = c(datasetID = "DeploymentIDX",
                                      datasetName = "DeploymentName",
                                      datafileSource = "rawDataFile"
                                      ),
    datapuppy.db.reqMetricFields = c(metricID = "MetricIDX",
                                     metricName = "MetricName"
                                     ),
    datapuppy.db.setTable = NULL,
    datapuppy.db.metricTable = NULL,
    datapuppy.db.dataTable = NULL,
    datapuppy.db.dsn = NULL

#    datapuppy.xmlNodes = list(root = "datapuppy", path = "defaultpath", files = "files", importdefs = "importdefs"),
#    datapuppy.elementNames = list(files = "file", importdefs = "importdef", corrections = "correction"),
#    datapuppy.fileAttributes = list(filename = "filename", deployment = "deployment", importdefs = "importdefs", checksum = "checksum"),
#    datapuppy.importDefAttributes = list(idx = "idx", datetime = "datetime")
  )
#  toset <- !(names(op.datapuppy) %in% names(op))
#  if(any(toset)) options(op.datapuppy[toset])
  options(op.datapuppy)
  invisible()
}


