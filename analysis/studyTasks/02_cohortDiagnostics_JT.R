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
install.packages('https://github.com/OHDSI/CohortDiagnostics/archive/refs/tags/v3.2.5.tar.gz')
# May only be needed once.


source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))


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
executionSettings <- list(
  projectName = tolower('SurgeryWaitTime'),
  cohortTable = tolower('jmt'),
  cdmDatabaseSchema = "cdm_531",
  vocabDatabaseSchema = "cdm_531",
  workDatabaseSchema = "jmt_surgerywaittime",
  dbms = "postgresql",
  cohortDatabaseSchema = "jmt_surgerywaittime",
  tablePrefix = "jmt_surgerywaittime",
  databaseName = "synpuf",
  cohortTable = "SurgeryWaitTime_synpuf"

)

outputFolder <- 'results'

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
