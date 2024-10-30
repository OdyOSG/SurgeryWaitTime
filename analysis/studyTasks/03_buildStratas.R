# A. File Info -----------------------

# Study:
# Task: Build Stratas


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
source("analysis/private/_utilities.R")
source("analysis/private/_buildStrata.R")


# C. Connection ----------------------

## Set connection block
# <<<
configBlock <- "synpuf"
# >>>

## Provide connection details
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
