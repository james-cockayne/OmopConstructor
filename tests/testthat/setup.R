
newSrc <- function(dbType = Sys.getenv("DB_TYPE", "duckdb")) {
  if (dbType == "duckdb") {
    CDMConnector::dbSource(
      con = duckdb::dbConnect(duckdb::duckdb()),
      writeSchema = "main"
    )
  } else {
    omopgenerics::newLocalSource()
  }
}
