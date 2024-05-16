# A. File Info -----------------------

# Task: Treatment History


# B. Functions ------------------------

## Helper functions -----------------------

getTreatmentHistory <- function(con,
                                workDatabaseSchema,
                                cohortTable,
                                targetId,
                                targetName,
                                treatmentCohorts,
                                thSettings,
                                outputFolder) {

  # Collect cohorts
  current_cohorts <- collectCohorts(con = con,
                                    workDatabaseSchema = workDatabaseSchema,
                                    cohortTable = cohortTable,
                                    targetId = targetId,
                                    eventIds = treatmentCohorts$id)

  tik <- Sys.time()

  # Run treatment history
  res <- doCreateTreatmentHistory(current_cohorts,
                                  targetCohortId = targetId,
                                  eventCohortIds = treatmentCohorts$id,
                                  periodPriorToIndex = thSettings$periodPriorToIndex,
                                  includeTreatments = thSettings$includeTreatments) %>%
    doEraDuration(minEraDuration = thSettings$minEraDuration) %>%
    doEraCollapse(eraCollapseSize = thSettings$eraCollapseSize) %>%
    doCombinationWindow(combinationWindow = thSettings$combinationWindow,
                        minPostCombinationDuration = thSettings$minPostCombinationDuration) %>%
    doFilterTreatments(filterTreatments = thSettings$filterTreatments) %>%
    postProcess(eventCohortIds = treatmentCohorts$id,
                eventCohortNames = treatmentCohorts$name,
                maxPathLength = thSettings$maxPathLength)

  res$duration_era <- as.integer(res$duration_era)

  # Job log
  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line("\nTreatment History built at: ", crayon::green(tok))
  cli::cat_line("Treatment History build took: ", crayon::green(tok_format))

  ## If res object is empty, skip export
  if (nrow(res) == 0) {
    return(NA)
  }

  # Export
  save_name <- paste("th", targetId,  sep = "_")
  save_path <- fs::path(outputFolder, save_name, ext = "csv")
  readr::write_csv(x = res, file = save_path)

  # Job log
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(save_path)), " to:", crayon::cyan(outputFolder), bullet = "info", bullet_col = "blue")
  cli::cat_line()

  invisible(res)
}


## Main function -----------------------

runTreatmentHistory <- function(con,
                                executionSettings,
                                analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder[1]) %>%
    fs::dir_create()

  targetCohorts <- analysisSettings$treatmentPatterns$cohorts$targetCohorts %>% dplyr::arrange(id)
  treatmentCohorts <- analysisSettings$treatmentPatterns$cohorts$eventCohorts %>% dplyr::arrange(id)
  thSettings <- analysisSettings$treatmentPatterns$treatmentHistorySettings

  # Job log
  cli::cat_boxx(crayon::magenta("Building Treatment History"))
  cli::cat_line()
  tik <- Sys.time()

  # Loop through target cohort ids
  for (i in seq_along(targetCohorts$id)) {

        # Target cohort id and name
        targetId <- targetCohorts$id[i]
        targetName <- targetCohorts$name[i]

        # Event cohort id and name
        eventCohorts <- treatmentCohorts %>%
          dplyr::select(id, name) %>%
          dplyr::mutate(type = "event")

        # Job log
        cli::cat_rule()
        txt1 <- paste0(targetName, " (id:", targetId, ")")
        cli::cat_bullet(crayon::green("Target Cohort: "), txt1, bullet = "pointer", bullet_col = "yellow")
        txt2 <- paste(eventCohorts$name, collapse = ", ")
        cli::cat_bullet(crayon::green("Event Cohorts: "), txt2, bullet = "pointer", bullet_col = "yellow")
        cli::cat_line()


        # Run treatment history
        getTreatmentHistory(con = con,
                            workDatabaseSchema = workDatabaseSchema,
                            cohortTable = cohortTable,
                            targetId = targetId,
                            targetName = targetName,
                            treatmentCohorts = eventCohorts,
                            thSettings = thSettings,
                            outputFolder = outputFolder)

  }

  # Job log
  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line()
  cli::cat_bullet("Execution took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  invisible(treatmentCohorts)
}

