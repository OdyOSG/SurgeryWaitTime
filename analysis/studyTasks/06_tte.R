# A. File Info -----------------------

# Study:
# Task: Time To Event


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(ggsurvfit)
library(survival)
library(survminer)
library(viridis)
library(scales)
source(here::here("analysis/private/_tte.R"))
source(here::here("analysis/private/_utilities.R"))


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
analysisSettings <- readSettingsFile(here::here("analysis/settings/tte.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/tte2.yml"))


# E. Script --------------------

# Time To Event (Whole Cohort)

executeTimeToEventSurvival(con = con,
                           executionSettings = executionSettings,
                           analysisSettings = analysisSettings)


## Time To Event (Create KM plots)

# Display limited to 365 days but all data preserved for flexibility
createKMplots(
  database = executionSettings$databaseName,
  con = con,
  executionSettings = executionSettings,
  max_display_days = 365  # Easy to change - try 730, 1000, etc.
)


## Time To Event (Only surgery patients)

executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings2)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
