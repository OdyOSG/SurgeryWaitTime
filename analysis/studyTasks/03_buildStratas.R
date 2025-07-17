# A. File Info -----------------------

# Study:
# Task: Build Stratas


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
source(here::here("analysis/private/_utilities.R"))
source(here::here("analysis/private/_buildStrata.R"))


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
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

## Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/strata.yml"))


# E. Script --------------------

## Build stratas

buildStrata(con = con,
            executionSettings = executionSettings,
            analysisSettings = analysisSettings)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
