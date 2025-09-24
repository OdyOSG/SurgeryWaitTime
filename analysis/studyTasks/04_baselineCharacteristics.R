# A. File Info -----------------------

# Study:
# Task: Baseline Characteristics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
source(here::here("analysis/private/_utilities.R"))
source(here::here("analysis/private/_conceptPrevalence.R"))


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
analysisSettings1 <- readSettingsFile(here::here("analysis/settings/baseline.yml"))
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/baseline2.yml"))
analysisSettings3 <- readSettingsFile(here::here("analysis/settings/baseline3.yml"))
analysisSettings4 <- readSettingsFile(here::here("analysis/settings/baseline4.yml"))


# E. Script --------------------

## Run concept characterization

### Default
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runDrugs = TRUE,
  runDemographics = TRUE,
  runContinuous = TRUE,
  runConditions = TRUE,
  runCohorts = TRUE,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings1
)

### Custom conditions
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCustom = TRUE,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings2
)

### BMI
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCohorts = TRUE,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings3
)

### Hospitalization
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCohorts = TRUE,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings4
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
