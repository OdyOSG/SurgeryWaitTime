# File to review cohort diagnostic results

# A. File info -------------

# The purpose of this file is to review Cohort Diagnostics results


# B. Dependencies ----------------------

library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(config)
library(CohortDiagnostics)
library(Ulysses)
library(fs)


# C. Script --------------------

databaseName <- "[block]" # The name of the database to use

## Path to cohort diagnostics results
dataFolder <- fs::path_abs("results") %>%
  fs::path(databaseName, "02_cohortDiagnostics")


## Add a scratch folder
scratchDiagnosticsFolder <- fs::path_abs("scratchDiagnostics") %>%  
  fs::dir_create()

## Path to sqlite db
sqlLiteDbPath <- fs::path(scratchDiagnosticsFolder, glue::glue("SWT_cd_{databaseName}"), ext = "sqlite")

# create merged results file
CohortDiagnostics::createMergedResultsFile(dataFolder = dataFolder,
                                           sqliteDbPath = sqlLiteDbPath)


## Launch diagnostics
CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath)

## NOTE: When done reviewing the shiny app hit the stop button in the console

