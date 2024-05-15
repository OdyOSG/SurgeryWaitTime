# A. File Info  -----------------------

# Study:
# Task: Build Cohorts
# Please refer to HowToRun.md in the documentation for instructions on
# running package

# B. Dependencies ----------------------
# Dependencies are handled by renv package.

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))


# C. Connection ----------------------

## Set connection block
# <<<
configBlock <- "synpuf"
# >>>

# ## Provide connection details
# connectionDetails <- DatabaseConnector::createConnectionDetails(
# dbms = config::get("dbms", config = configBlock),
# user = config::get("user", config = configBlock),
# password = config::get("password", config = configBlock),
# connectionString = config::get("connectionString", config = configBlock),
# )

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  server = config::get("server", config = configBlock),
  port = "5441"
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

# # Needed to execute on Postgres, will be moved in final.
# executionSettings$projectName = tolower(executionSettings$projectName)
# executionSettings$cohortTable = tolower(executionSettings$cohortTable)
# executionSettings$workDatabaseSchema = tolower(executionSettings$workDatabaseSchema)


# E. Script --------------------

## Initialize cohort tables

initializeCohortTables(executionSettings = executionSettings, con = con,
                       dropTables = TRUE)

## Generate cohorts

generatedCohorts <- generateCohorts(
  executionSettings = executionSettings,
  con = con,
  cohortManifest = cohortManifest,
  outputFolder = outputFolder
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(connection = con)
