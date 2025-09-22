
source("/code/buildObservationPeriod.R")
source("/code/collapseRecords.R")

library(CDMConnector)
library(DatabaseConnector)

connectionDetails <-
    DatabaseConnector::createConnectionDetails(
        dbms = Sys.getenv("DBMS"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        server = Sys.getenv("DB_SERVER"),
        port = Sys.getenv("DB_PORT"),
        extraSettings = Sys.getenv("SETTINGS"),
        pathToDriver = Sys.getenv("PATH_TO_DRIVER"))

con <- DatabaseConnector::connect(connectionDetails)

cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = Sys.getenv("CDM_SCHEMA"),
  writeSchema = Sys.getenv("RESULTS_SCHEMA"),
  cdmName = "DB"
)

if (Sys.getenv("collapseDays") == "" & Sys.getenv("collapseEra") != "") {
  Sys.setenv(collapseDays = Sys.getenv("collapseEra"))
}
if (Sys.getenv("persistenceDays") == "" & Sys.getenv("persistenceWindow") != "") {
  Sys.setenv(persistenceDays = Sys.getenv("persistenceWindow"))
}

buildObservationPeriod(
  cdm,
  collapseDays = as.numeric(Sys.getenv("collapseDays")),
  persistenceDays = as.numeric(Sys.getenv("persistenceDays"))
)
