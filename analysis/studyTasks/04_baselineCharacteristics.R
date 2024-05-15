# A. File Info -----------------------

# Study:
# Task: Baseline Characteristics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
source("analysis/private/_utilities.R")
source("analysis/private/_conceptPrevalence.R")
source("analysis/private/_conditionRollup.R")


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
analysisSettings <- readSettingsFile(here::here("analysis/settings/baseline.yml"))


# E. Script --------------------

## Run concept characterization

executeConceptCharacterization(con = con,
                               type = "baseline",
                               runDrugs = FALSE,
                               runDemographics = FALSE,
                               runContinuous = FALSE,
                               runCohorts = TRUE,
                               runConditions = FALSE,
                               executionSettings = executionSettings,
                               analysisSettings = analysisSettings)


# ## Run ICD chapters rollup
#
# executeConditionRollup(con = con,
#                        type = "baseline",
#                        executionSettings = executionSettings,
#                        analysisSettings = analysisSettings)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
