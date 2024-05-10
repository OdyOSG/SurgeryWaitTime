# A. File Info -----------------------

# Study:
# Task: Post-Index Utilization


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
# added createGrid and createGrid2 to utilities.R
source("analysis/private/_utilities.R")
source("analysis/private/_conceptPrevalence.R")


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

outputFolder <- 'results_simple'

## Analysis Settings
#Changed id to targetId
analysisSettings <- readSettingsFile(here::here("analysis/settings/postIndex.yml"))


# E. Script --------------------

#startSnowflakeSession(con = con, executionSettings = executionSettings)

print(outputFolder)
## Get Post-Index Covariates

executeConceptCharacterization(con = con,
                               type = "postIndex",
                               runDrugs = TRUE,
                               executionSettings = executionSettings,
                               analysisSettings = analysisSettings)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)

