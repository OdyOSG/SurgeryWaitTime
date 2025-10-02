# A. File Info -----------------------

# Study: Surgery Wait Time
# Task: Baseline Characteristics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = T)
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

## Run concept and cohort characterization

### Default
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runDrugs = T,
  runDemographics = T,
  runContinuous = T,
  runConditions = T,
  runCohorts = T,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings1
)

### Custom conditions
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCustom = T,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings2
)

### BMI
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCohorts = T,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings3
)

### Hospitalization
executeConceptCharacterization(
  con = con,
  type = "baseline",
  runCohorts = T,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings4
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
