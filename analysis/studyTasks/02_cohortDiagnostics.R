# A. File Info -----------------------

# Task: Cohort Diagnostics
# Please refer to HowToRun.md in the documentation for instructions on
# running package

# B. Dependencies ----------------------
# Dependencies are handled by renv package.

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
# May only be needed once.


source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))


# C. Connection ----------------------

## Set connection Block
# <<<
configBlock <- "[block]"
# >>>

## Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

## Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Study Variables -----------------------

## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at( c("dbms", "user", "password", "connectionString"))

# Needed to execute on Postgres, will be moved in final.
executionSettings$projectName = tolower(executionSettings$projectName)
executionSettings$cohortTable = tolower(executionSettings$cohortTable)
executionSettings$workDatabaseSchema = tolower(executionSettings$workDatabaseSchema)

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "02_cohortDiagnostics") %>%
  fs::dir_create()

## Add study variables or load from settings
diagCohorts <- getCohortManifest() %>%
  dplyr::filter(type == "target")


# E. Script --------------------

## Run cohort diagnostics
runCohortDiagnostics(executionSettings = executionSettings,
                     con = con,
                     cohortManifest = diagCohorts,
                     outputFolder = outputFolder)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
