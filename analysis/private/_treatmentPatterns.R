# A. File Info -----------------------

# Task: Treatment Patterns


# B. Functions ------------------------


## Time to Event -----------------------


prepTte <- function(con,
                    th,
                    strata = NULL,
                    workDatabaseSchema,
                    cohortTable,
                    targetCohort) {

  # Job log
  cli::cat_line(crayon::blue("Extracting Survival Table..."))

  # Get target cohort id and name
  targetCohortIds <- targetCohort$id
  targetCohortNames <- targetCohort$name

  # Get target cohort table data
  sql <- "SELECT * FROM @write_schema.@cohort_table
          WHERE cohort_definition_id IN (@target_cohort_id);"  %>%
    SqlRender::render(
      write_schema = workDatabaseSchema,
      cohort_table = cohortTable,
      target_cohort_id = targetCohortIds
    ) %>%
    SqlRender::translate(con@dbms)

  targetTbl <- DatabaseConnector::querySql(connection = con, sql = sql)

  colnames(targetTbl) <- tolower(colnames(targetTbl))

  # Convert treatment history table to tibble
  dt <- th %>% tibble::as_tibble()

  # Create empty list object
  survDat <- vector('list', length(targetCohortIds))

  # Loop through target cohort ids
  for (i in seq_along(targetCohortIds)) {

    tte <- targetTbl %>%
      # filter to cohort id
      dplyr::filter(cohort_definition_id == targetCohortIds[i]) %>%
      # join th and target table to determine censoring
      dplyr::inner_join(th, by = c("subject_id" = "person_id"), relationship = "many-to-many") %>%
      # identifying the event and convert time to years
      dplyr::mutate(
        event = dplyr::case_when(
          event_end_date < cohort_end_date ~ 1,
          TRUE ~ 0
        ),
        time_years = duration_era / 365.25
      ) %>%
      dplyr::select(event_cohort_name, duration_era, event_cohort_id, time_years, event)


    # Add strata to object - Not sure if we need this
    if (is.null(strata)) {

      strata_tbl <- tte %>%
        dplyr::mutate(strata = 1)

    } else {

      checkmate::assert_class(strata, "strata")

      strata_tbl <- tte %>%
        dplyr::left_join(strata$data, by = c("subject_id" = "rowId"))
    }


    # t2 <- strata_tbl %>%
    #   dplyr::count(event_cohort_name) %>%
    #   dplyr::arrange(desc(n)) %>%
    #   dplyr::filter(n >= 15) %>%
    #   dplyr::select(event_cohort_name)
    #
    # survTab <- strata_tbl %>%
    #   dplyr::inner_join(t2, by = c("event_cohort_name")) %>%
    #   dplyr::mutate(outcome = paste(event_cohort_name, strata, sep = "_"))

    survTab <- strata_tbl

    survFit <- survival::survfit(
      #survival::Surv(duration_era, event) ~ outcome,
      survival::Surv(duration_era, event) ~ event_cohort_name,
      data = survTab
    )

    # Remove string "outcome=" from strata variable
    names(survFit$strata) <- gsub("outcome=", "", names(survFit$strata))

    # Export
    survival_info <- list(
      data = survTab,
      fit = survFit)

  }

  return(survival_info)
}


## Main function -----------------------

executeTimeToEvent <- function(con,
                               executionSettings,
                               analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder)
  thHistoryFolder <- outputFolder[1]   # hard coded to get th folder
  tteFolder <- outputFolder[2]         # hard coded to get tte folder

  # Get target cohort ids and names
  targetCohorts <- analysisSettings$treatmentPatterns$cohorts$targetCohort

  # List all treatment history files
  thFiles <- fs::dir_ls(thHistoryFolder, recurse = TRUE, type = "file")


  # Loop through treatment history(th.csv) files
  for (i in seq_along(thFiles)) {

    # Get the target cohort id out of the th_<cohort_id>.csv file name
    cohortId <- tools::file_path_sans_ext(basename(thFiles[i])) %>%
      gsub("th_", "", .) %>%
      as.integer(.)

    targetCohort <- targetCohorts %>% dplyr::filter(id == cohortId)

    # If there is no th file for the target cohort, break the loop and continue with the next target cohort id
    if (nrow(targetCohort) == 0) {

      cli::cat_bullet("No 'th' file detected for cohort id: ", crayon::red(cohortId), ". Function will continue with the next cohort id.",
                      bullet = "info", bullet_col = "blue")
      next
    }

    # Job log
    cli::cat_line()
    cli::cat_boxx(crayon::magenta("Building Time to Event"))
    cli::cat_line()
    cli::cat_bullet("Execute time to event for cohort id: ", crayon::magenta(targetCohort$id),
                    " (Cohort name: ",  crayon::magenta(targetCohort$name), ")",
                    bullet = "pointer", bullet_col = "yellow")

    # Read csv file
    th <- readr::read_csv(file = thFiles[i], show_col_types = FALSE)

    # Get time to event data frame
    tteDat <- prepTte(con = con,
                      th = th,
                      workDatabaseSchema = workDatabaseSchema,
                      cohortTable = cohortTable,
                      targetCohort = targetCohort)


    # Create tte folder
    save_path <- fs::path(here::here(tteFolder)) %>%
      fs::dir_create()

    # Export
    file_name <- paste("tte", cohortId, sep = "_")
    save_path2 <- fs::path(save_path, file_name, ext = "rds")
    readr::write_rds(tteDat, file = save_path2)

    # Job log
    cli::cat_line()
    cli::cat_bullet("Saved file ", crayon::green(basename(save_path2)), " to: ", crayon::green(save_path), bullet = "info", bullet_col = "blue")
    cli::cat_line()

  }

  invisible(tteDat)
}

