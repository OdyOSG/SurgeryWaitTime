# A. File Info  -----------------------

# Task: Build Cohorts


# B. Dependencies ----------------------

## Load libraries and scripts

library(dplyr)
source(paste0('analysis/private/_buildCohorts.R'))
source(paste0('analysis/private/_executeStudy.R'))
source(paste0('analysis/private/_utilities.R'))

# May only be needed once.
#install.packages('https://github.com/OHDSI/MethodEvaluation/archive/refs/tags/v2.3.0.tar.gz')
#install.packages('https://github.com/OHDSI/CohortMethod/archive/refs/tags/v5.2.1.tar.gz')
#install.packages('https://github.com/OHDSI/ROhdsiWebApi/archive/refs/tags/v1.3.3.tar.gz')
#install.packages('https://github.com/OHDSI/CohortDiagnostics/archive/refs/tags/v2.1.3.tar.gz')
#install.packages('https://github.com/OHDSI/CirceR/archive/refs/tags/v1.3.2.tar.gz')
#install.packages('https://github.com/OHDSI/CohortGenerator/archive/refs/tags/v0.8.1.tar.gz')
# C. Connection ----------------------

## Set connection block
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
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "01_buildCohorts") %>%
  fs::dir_create()

## Load cohorts
cohortManifest <- getCohortManifest()

# Needed to execute on Postgres, will be moved in final.
executionSettings$projectName = tolower(executionSettings$projectName)
executionSettings$cohortTable = tolower(executionSettings$cohortTable)
executionSettings$workDatabaseSchema = tolower(executionSettings$workDatabaseSchema)

# E. Script --------------------

### RUN ONCE - Initialize Cohort table ###
initializeCohortTables(executionSettings = executionSettings, con = con, dropTables = TRUE)

## Generate cohorts

generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(connection = con)
