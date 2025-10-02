# A. File Info -----------------------

# Study: Surgery Wait Time
# Task: Time To Event


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = T)
library(DatabaseConnector)
library(ggsurvfit)
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
# Single Curve
analysisSettings_col_single <- readSettingsFile(here::here("analysis/settings/tte_col_single.yml"))
analysisSettings_breast_single <- readSettingsFile(here::here("analysis/settings/tte_breast_single.yml"))
analysisSettings_lung_single <- readSettingsFile(here::here("analysis/settings/tte_lung_single.yml"))
analysisSettings_eso_single <- readSettingsFile(here::here("analysis/settings/tte_eso_single.yml"))

# Multiple Curves
analysisSettings_col_multiple <- readSettingsFile2(here::here("analysis/settings/tte_col_multiple.yml"))
analysisSettings_lung_multiple <- readSettingsFile2(here::here("analysis/settings/tte_lung_multiple.yml"))
analysisSettings_breast_multiple <- readSettingsFile2(here::here("analysis/settings/tte_breast_multiple.yml"))
analysisSettings_eso_multiple <- readSettingsFile2(here::here("analysis/settings/tte_eso_multiple.yml"))

# Time To Event
analysisSettings2 <- readSettingsFile(here::here("analysis/settings/tte2.yml"))


# E. Script --------------------


## Time To Event - Multiple Curves --------------------

## Colorectal
executeSurvivalAnalysisMultipleCurves(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_col_multiple
)

## Lung
executeSurvivalAnalysisMultipleCurves(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_lung_multiple
)

## Breast
executeSurvivalAnalysisMultipleCurves(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_breast_multiple
)

## Esophagus
executeSurvivalAnalysisMultipleCurves(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_eso_multiple
)

## Kaplan-Meier plots
createKMplotsMultipleCurves(database = executionSettings$databaseName)


## Time To Event - Single Curve --------------------

## Colorectal
executeSurvivalAnalysisSingleCurve(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_col_single
)

## Lung
executeSurvivalAnalysisSingleCurve(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_lung_single
)

## Esophagus
executeSurvivalAnalysisSingleCurve(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_eso_single
)

## Breast
executeSurvivalAnalysisSingleCurve(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings_breast_single
)

## Kaplan-Meier plots
createKMplotsSingleCurve(database = executionSettings$databaseName)


## Time To Event (Only surgery patients) --------------------

executeTimeToEvent(
  con = con,
  executionSettings = executionSettings,
  analysisSettings = analysisSettings2
)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
