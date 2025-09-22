
library(DatabaseConnector)
library(CDMConnector)
library(OmopConstructor)

pathToDriver <- tempdir()

downloadJdbcDrivers(dbms = Sys.getenv("DBMS"), pathToDriver = pathToDriver)

connectionDetails <- createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  server = Sys.getenv("DB_SERVER"),
  port = Sys.getenv("DB_PORT"),
  extraSettings = Sys.getenv("SETTINGS"),
  pathToDriver = pathToDriver
)

con <- connect(connectionDetails = connectionDetails)

cdm <- cdmFromCon(
  con = con,
  cdmSchema = Sys.getenv("CDM_SCHEMA"),
  writeSchema = Sys.getenv("RESULTS_SCHEMA"),
  cdmName = "TempName"
)

if (Sys.getenv("collapseDays") == "" & Sys.getenv("collapseEra") != "") {
  Sys.setenv(collapseDays = Sys.getenv("collapseEra"))
}
if (Sys.getenv("persistenceDays") == "" & Sys.getenv("persistenceWindow") != "") {
  Sys.setenv(persistenceDays = Sys.getenv("persistenceWindow"))
}

cdm <- buildObservationPeriod(
  cdm = cdm,
  collapseDays = as.numeric(Sys.getenv("collapseDays")),
  persistenceDays = as.numeric(Sys.getenv("persistenceDays"))
)

cdmDisconnect(cdm = cdm)
