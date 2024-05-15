# A. File Info -----------------------

# Study:
# Task: Time To Event


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_treatmentHistory.R")
source("analysis/private/_treatmentHistory_helpers.R")
source("analysis/private/_treatmentPatterns.R")
source("analysis/private/_utilities.R")


# C. Connection ----------------------

## Set connection block
# <<<
configBlock <- "synpuf"
# >>>

# ## Provide connection details
# connectionDetails <- DatabaseConnector::createConnectionDetails(
#   dbms = config::get("dbms", config = configBlock),
#   user = config::get("user", config = configBlock),
#   password = config::get("password", config = configBlock),
#   connectionString = config::get("connectionString", config = configBlock)
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
  purrr::discard_at(c("dbms", "user", "password", "connectionString"))

## Analysis Settings
analysisSettings <- readSettingsFile(here::here("analysis/settings/tte.yml"))


# E. Script --------------------

# Run treatment history

runTreatmentHistory(con = con,
                    executionSettings = executionSettings,
                    analysisSettings = analysisSettings)

# Get time to discontinuation

executeTimeToEvent(con = con,
                   executionSettings = executionSettings,
                   analysisSettings = analysisSettings)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
