# A. File Info  -----------------------

# Task: Build Cohorts
# Please refer to HowToRun.md in the documentation for instructions on
# running package

# B. Dependencies ----------------------
# Dependencies are handled by renv package.

## Load libraries and scripts

library(dplyr)
source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))
#install.packages('https://github.com/OHDSI/CohortGenerator/archive/refs/tags/v0.8.1.tar.gz')
#install.packages('https://github.com/OHDSI/CirceR/archive/refs/tags/v1.3.3.tar.gz')
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
# databaseName: synpuf
# cohortTable: SurgeryWaitTime_synpuf
## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

outputFolder <- 'results'

## Load cohorts
cohortManifest <- getCohortManifest()

# Needed to execute on Postgres, will be moved in final.
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
