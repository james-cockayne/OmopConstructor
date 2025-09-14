
dbToTest <- Sys.getenv("DB_TO_TEST", "duckdb CDMConnector")
copyCdm <- function(cdm) {
  # create the source to copy the cdm to
  prefix <- "oc_test_"
  to <- switch(
    dbToTest,
    "duckdb CDMConnector" = CDMConnector::dbSource(
      con = duckdb::dbConnect(drv = duckdb::duckdb(dbdir = ":memory:")),
      writeSchema = c(schema = "main", prefix = prefix)
    ),
    "sql server CDMConnector" = NULL,
    "redshift CDMConnector" = NULL,
    "postgres CDMConnector" = NULL,
    "local" = omopgenerics::newLocalSource()
  )

  # insert cdm to my source of interest
  cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = to)

  return(cdm)
}
dropCreatedTables <- function(cdm) {
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::everything())
}
