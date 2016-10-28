.onLoad <- function(libname, pkgname) {
  op = options()
  op.datapuppy = list(
    datapuppy.connections = c("MySQLConnection")
  )

#
#   # THE FOLLOWING OPTIONS WERE USED WHEN DATAPUPPY WAS GOING TO WRITE PROVENANCE
# # TO XML.  I DON'T THINK THEY ARE USEFUL ANYMORE.
#
# #   op <- options()
# #   op.datapuppy <- list(
# #     datapuppy.path = "~/R-dev",
# #     datapuppy.install.args = "",
# #     datapuppy.name = "Geoffrey Poole",
# #     datapuppy.desc.author = '"Geoffrey Poole <gpoole@montana.edu> [aut, cre]"',
# #     datapuppy.desc.license = "What license is it under?",
# #     datapuppy.desc.suggests = NULL,
# #     datapuppy.desc = list(),
# #
# #     datapuppy.root = "datapuppy",
# #     datapuppy.tweaks = "tweaks",
# #     datapuppy.keys = "foreignkeys",
# #     datapuppy.tweakorders = "tweakorders",
# #     datapuppy.why = "comment",
# #     datapuppy.orders = "ordering",
# #
# #     datapuppy.path = "basepath",
# #     datapuppy.files = "datafiles",
# #     datapuppy.impdefs = "importdefs",
# #     datapuppy.sets = "datasets",
# #     datapuppy.sets.keys = "foreignkeys",
# #     datapuppy.sets.tweaks = "tweaks",
# #     datapuppy.sets.tweakorders = "tweakorders",
# #     datapuppy.sets.tweaks.attr = list(activate = "added", deactivate = "removed"),
# #
# #     datapuppy.db.reqObservationFields = c(observationID = "ObservationIDX",
# #                                           observationDateTime = "ObservationTime",
# #                                           datasetID = "DeploymentIDX",
# #                                           metricID = "MetricIDX",
# #                                           value = "Value"
# #                                           ),
# #     datapuppy.db.reqDatasetFields = c(datasetID = "DeploymentIDX",
# #                                       datasetName = "DeploymentName",
# #                                       datafileSource = "rawDataFile"
# #                                       ),
# #     datapuppy.db.reqMetricFields = c(metricID = "MetricIDX",
# #                                      metricName = "MetricName"
# #                                      ),
# #     datapuppy.db.setTable = NULL,
# #     datapuppy.db.metricTable = NULL,
# #     datapuppy.db.dataTable = NULL,
# #     datapuppy.db.dsn = NULL
# #   )
   options(op.datapuppy)
   invisible()
}
#
#
