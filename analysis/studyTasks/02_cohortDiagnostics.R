# A. File Info -----------------------

# Task: Cohort Diagnostics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_buildCohorts.R")
source("analysis/private/_utilities.R")


# C. Connection ----------------------

## Set connection Block
# <<<
configBlock <- "synpuf"
# >>>

## Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = "ohdsi",
  password = "ohdsi",
  server = "testnode.arachnenetwork.com/synpuf_110k",
  port = 5441
)

## Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Study Variables -----------------------

## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at( c("dbms", "user", "password", "connectionString"))

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
