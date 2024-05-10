# A. File Info -----------------------

# Study:
# Task: Build Stratas


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
source("analysis/private/_utilities.R")
source("analysis/private/_buildStrata.R")


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
outputFolder <- 'results'

# D. Variables -----------------------

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
## Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/strata.yml"))


# E. Script --------------------

#startSnowflakeSession(con =con, executionSettings = executionSettings)

## Build stratas

buildStrata(con = con,
            executionSettings = executionSettings,
            analysisSettings = analysisSettings)


# F. Session Info ------------------------

DatabaseConnector::disconnect(con)
