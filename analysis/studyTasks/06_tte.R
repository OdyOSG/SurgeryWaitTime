# A. File Info -----------------------

# Study:
# Task: Time To Event


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(ggsurvfit)
source("analysis/private/_tte.R")
source("analysis/private/_utilities.R")


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
analysisSettings <- readSettingsFile(here::here("analysis/settings/tte.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/tte2.yml"))


# E. Script --------------------

# Time To Event (Whole Cohort)
debug(executeTimeToEventSurvival)
executeTimeToEventSurvival(con = con,
                           executionSettings = executionSettings,
                           analysisSettings = analysisSettings)


## Time To Event (Create KM plots)

createKMplots(database = configBlock)


## Time To Event (Only surgery patients)

executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings2)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
