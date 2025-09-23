
# export duckDB database
cdm <- omock::mockCdmFromDataset(datasetName = "GiBleed", source = "local")
loc <- file.path(getwd(), "docker", "scripts", "gibleed.duckdb")
con <- duckdb::dbConnect(drv = duckdb::duckdb(dbdir = loc))
DBI::dbExecute(conn = con, statement = "CREATE SCHEMA cdm")
to <- CDMConnector::dbSource(con = con, writeSchema = "cdm")
cdm <- omopgenerics::insertCdmTo(cdm = cdm, to = to)
CDMConnector::cdmDisconnect(cdm = cdm)

# create env file
loc <- file.path(getwd(), "docker", "scripts", "duckdb.env")
content <- c(
  "DB_SERVER=gibleed.duckdb",
  "DBMS=duckdb",
  "CDM_SCHEMA=cdm",
  "RESULTS_SCHEMA=main",
  "persistenceDays=180",
  "collapseDays=180"
)
writeLines(text = content, con = loc)

print(list.files(file.path(getwd(), "docker", "scripts")))
