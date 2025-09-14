
dbToTest <- Sys.getenv("DB_TO_TEST", "duckdb CDMConnector")
copyCdm <- function(cdm) {
  # random prefix that starts with oc
  prefix <- paste0("oc_", paste0(sample(letters, 3, T), collapse = TRUE), "_")

  # create the source to copy the cdm to
  to <- switch(
    dbToTest,
    "duckdb CDMConnector" = CDMConnector::dbSource(
      con = duckdb::dbConnect(drv = duckdb::duckdb(dbdir = ":memory:")),
      writeSchema = c(schema = "main", prefix = prefix)
    ),
    "sql server CDMConnector" = NULL,
    "redshift CDMConnector" = NULL,
    "postgres CDMConnector" = CDMConnector::dbSource(
      con = RPostgres::dbConnect(
        RPostgres::Postgres(),
        dbname = stringr::str_split_1(Sys.getenv("CDM5_POSTGRESQL_SERVER"), "/")[2],
        host = stringr::str_split_1(Sys.getenv("CDM5_POSTGRESQL_SERVER"), "/")[1],
        user = Sys.getenv("CDM5_POSTGRESQL_USER"),
        password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
      ),
      writeSchema = c(schema = "public", prefix = prefix)
    ),
    "local omopgenerics" = omopgenerics::newLocalSource()
  )

  # insert cdm to my source of interest
  cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = to)

  return(cdm)
}
dropCreatedTables <- function(cdm) {
  omopgenerics::dropSourceTable(cdm = cdm, name = dplyr::everything())
  omopgenerics::cdmDisconnect(cdm = cdm)
}
