# A. File info -------------

# Study:
# Task: Review Cohort Diagnostics shiny app


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(CohortDiagnostics)
library(Ulysses)
library(fs)


# C. Script --------------------

## Insert database name
databaseName <- ""

## Path to cohort diagnostics results
dataFolder <- fs::path_abs("results") %>%
  fs::path(databaseName, "02_cohortDiagnostics")

## Add a scratch folder
scratchDiagnosticsFolder <- fs::path_abs("scratchDiagnostics") %>%
  fs::dir_create()

## Path to sqlite db
sqlLiteDbPath <- fs::path(scratchDiagnosticsFolder, glue::glue("SWT_cd_{databaseName}"), ext = "sqlite")

## Create merged results file
CohortDiagnostics::createMergedResultsFile(dataFolder = dataFolder,
                                           sqliteDbPath = sqlLiteDbPath)


## Launch Cohort Diagnostics shiny app
CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath)

## NOTE: When done reviewing the shiny app hit the stop button in the console

