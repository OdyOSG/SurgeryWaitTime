# A. File Info -----------------------

# Study: Surgery Wait Time
# Task: Build Cohorts


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = T)
library(DatabaseConnector)
source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))


# C. Connection ----------------------

## Set connection block
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


# D. Variables -----------------------

## Administrative Variables
executionSettings <- config::get(config = configBlock) %>%
  purrr::discard_at(c("user", "password", "connectionString"))

outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "01_buildCohorts") %>%
  fs::dir_create()

## Load cohorts
cohortManifest <- getCohortManifest()


# E. Script --------------------

## Initialize cohort tables

initializeCohortTables(
  executionSettings = executionSettings,
  con = con,
  dropTables = F
)

## Generate cohorts

generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(connection = con)
