# A. File Info  -----------------------

# Task: Build Cohorts


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_buildCohorts.R")
source("analysis/private/_utilities.R")


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
