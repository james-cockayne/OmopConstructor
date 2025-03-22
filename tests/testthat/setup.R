
src <- CDMConnector::dbSource(
  con = duckdb::dbConnect(duckdb::duckdb()),
  writeSchema = "main"
)
