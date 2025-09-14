
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
    "sqlserver CDMConnector" = CDMConnector::dbSource(
      con = odbc::dbConnect(
        odbc::odbc(),
        Driver = "ODBC Driver 18 for SQL Server",
        Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
        Database = "CDMV5",
        UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
        PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
        TrustServerCertificate="yes",
        Port = 1433
      ),
      writeSchema = c(catalog = "ohdsi", schema = "dbo", prefix = prefix)
    ),
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
