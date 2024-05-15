# A. File Info -----------------------

# Task: Build Cohorts


# B. Functions ------------------------

initializeCohortTables <- function(executionSettings,
                                   con,
                                   dropTables = FALSE) {

  ## Create cohort tables names' to create or drop
  name <- executionSettings$cohortTable
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = executionSettings$cohortTable)

  ## Drop cohort tables
  if (dropTables == TRUE) {

    ## Delete csv files from "01_buildCohorts" folder
    manifestPath <- here::here("results", executionSettings$databaseName, "01_buildCohorts")
    pathFiles <- list.files(manifestPath,  full.names = TRUE)
    sapply(pathFiles, unlink)

    cli::cat_line("Dropping cohort tables")

    for (i in 1:length(cohortTableNames)) {

      sql <- "DROP TABLE IF EXISTS @writeSchema.@tableName;"

      dropSql <- SqlRender::render(
        sql,
        writeSchema = executionSettings$workDatabaseSchema,
        tableName = cohortTableNames[i]
      ) %>%
        SqlRender::translate(targetDialect = executionSettings$dbms)

      DatabaseConnector::executeSql(connection = con, dropSql, progressBar = FALSE)

      cli::cat_bullet(paste0("Dropped table ", crayon::cyan(cohortTableNames[i])), bullet = "en_dash")

    }


  }

  ## Create cohort tables
  CohortGenerator::createCohortTables(connection = con,
                                      cohortDatabaseSchema = executionSettings$workDatabaseSchema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)

  invisible(cohortTableNames)
}


prepManifestForCohortGenerator <- function(cohortManifest) {

  # Add JSON file location as variable
  cohortsToCreate <- cohortManifest %>%
    dplyr::mutate(
      json = purrr::map_chr(file, ~readr::read_file(.x))
    ) %>%
    dplyr::select(id, name, json) %>%
    dplyr::rename(cohortId = id, cohortName = name)

  # Add SQL code out of JSON file (using CirceR)
  cohortsToCreate$sql <- purrr::map_chr(
    cohortsToCreate$json,
    ~CirceR::buildCohortQuery(CirceR::cohortExpressionFromJson(.x),
                              CirceR::createGenerateOptions(generateStats = TRUE)))

  return(cohortsToCreate)
}


generateCohorts <- function(executionSettings,
                            con,
                            cohortManifest,
                            outputFolder) {

  # Get JSON definition and SQL code of cohorts (using CirceR)
  cohortsToCreate <- prepManifestForCohortGenerator(cohortManifest)

  # Path to save records of executed cohort definitions
  incrementalFolder <- fs::path(outputFolder)

  # Create cohort tables names' for function CohortGenerator::generateCohortSet
  name <- executionSettings$cohortTable
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = executionSettings$cohortTable)

  # Generate cohorts
  cohortStatus <- CohortGenerator::generateCohortSet(
                    connection = con,
                    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
                    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
                    cohortTableNames = cohortTableNames,
                    cohortDefinitionSet = cohortsToCreate,
                    incremental = TRUE,
                    incrementalFolder = incrementalFolder
                  )

  # Insert inclusion/exclusion rules
  CohortGenerator::insertInclusionRuleNames(
    connectionDetails = connectionDetails,
    cohortDefinitionSet = cohortsToCreate,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cohortInclusionTable = cohortTableNames$cohortInclusionTable
  )

 # Export inclusion/exclusion rules' statistics
 CohortGenerator::exportCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortStatisticsFolder = fs::path(outputFolder, "inclusionStats"),
    databaseId = executionSettings$databaseName
  )

  # Export: cohort generation status (with execution times)
  savePath <- fs::path(outputFolder, "cohortGenerationStatus.csv")
  readr::write_csv(x = cohortStatus, file = savePath)

  # Get cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(
    connection = con,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable,
    cohortIds = cohortsToCreate %>% dplyr::pull(cohortId),
    cohortDefinitionSet = cohortsToCreate,
    databaseId = executionSettings$databaseName
  ) %>%
    dplyr::select(cohortId, cohortName, cohortEntries, cohortSubjects, databaseId)


  # Format
  tb <- cohortManifest %>%
    dplyr::left_join(cohortCounts, by = c("id" = "cohortId")) %>%
    dplyr::rename(
      entries = cohortEntries,
      subjects = cohortSubjects,
      database = databaseId
      ) %>%
    dplyr::select(id, name, type, entries, subjects, file, database)

  # Export: cohort counts
  savePath <- fs::path(outputFolder, "cohortManifest.csv")
  readr::write_csv(x = tb, file = savePath)

  cli::cat_bullet("Cohort counts saved to ", crayon::cyan(savePath), bullet = "tick", bullet_col = "green")

  return(cohortCounts)
}

# Run Cohort Diagnostis
# Description: this function is used to run cohort diagnostics. preps cohorts for run
# and then executes cohort diagnostics
# inputs:
# con -> the connection objection accessing the dbms storing the omop data
# executionSettings -> the DatabaseSchema information required to run the study
# cohortManifest -> the set of cohorts used to run diagnostics
# outputFolder -> the save location of the cohort diagnostics run
# return:
# invisible return of the cohortsToRun for diagnostics. The output of this function
# is a folder with cohort diagnostics run.

runCohortDiagnostics <- function(con,
                                 executionSettings,
                                 cohortManifest,
                                 outputFolder) {

  cohortsToRun <- prepManifestForCohortGenerator(cohortManifest) %>%
    dplyr::mutate(
      cohortId = as.numeric(cohortId)
    )

  name <- executionSettings$cohortTable

  cohortTableNames <- list(cohortTable = paste0(name),
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))


  # Run cohort diagnostics
  CohortDiagnostics::executeDiagnostics(
    cohortDefinitionSet = cohortsToRun,
    exportFolder = outputFolder,
    cohortTableNames = cohortTableNames,
    cohortDatabaseSchema = executionSettings$workDatabaseSchema,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    vocabularyDatabaseSchema = executionSettings$vocabDatabaseSchema,
    databaseId = executionSettings$databaseName,
    connection = con,
    incremental = TRUE,
    minCellCount = 5
  )

  cli::cat_bullet("Saving Cohort Diagnostics to ", crayon::cyan(outputFolder), bullet = "tick", bullet_col = "green")

  invisible(cohortsToRun)
}
