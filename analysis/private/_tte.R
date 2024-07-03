# A. File Info -----------------------

# Task: Treatment History


# B. Functions ------------------------

## Helper functions -----------------------

collectCohorts <- function(con,
                           workDatabaseSchema,
                           cohortTable,
                           targetId,
                           eventId) {

  sql <- "
          WITH e AS (
            SELECT *
            FROM @write_schema.@cohort_table
            WHERE cohort_definition_id in (@eventId)
          )
          SELECT
            e.cohort_definition_id as event_id,
            t.cohort_definition_id as target_id,
            e.cohort_start_date as event_start_date,
            e.cohort_end_date as event_end_date,
            t.cohort_start_date as target_start_date,
            t.cohort_end_date as target_end_date,
            t.subject_id,
            RANK() OVER(PARTITION BY t.subject_id ORDER BY e.cohort_start_date ASC) as rank,
            DATEDIFF(day, t.cohort_start_date, e.cohort_start_date) as diff
          FROM (
            SELECT *
            FROM @write_schema.@cohort_table
            WHERE cohort_definition_id = @targetId
          ) t
          LEFT JOIN e ON
            e.subject_id = t.subject_id AND
            e.cohort_start_date >= t.cohort_start_date AND
            e.cohort_start_date <= t.cohort_end_date;"


  renderedSql <- SqlRender::render(
    sql = sql,
    write_schema = workDatabaseSchema,
    cohort_table = cohortTable,
    targetId = targetId,
    eventId = eventId
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  current_cohorts <- DatabaseConnector::querySql(connection = con, sql = renderedSql)

  names(current_cohorts) <- tolower(names(current_cohorts))
  current_cohorts <- data.table::as.data.table(current_cohorts)
  current_cohorts <- current_cohorts %>% dplyr::filter(rank == 1)

  return(current_cohorts)
}


collectCohorts2 <- function(con,
                            workDatabaseSchema,
                            cohortTable,
                            targetId,
                            eventId) {

  sql <- "
          WITH e AS (
            SELECT *
            FROM @write_schema.@cohort_table
            WHERE cohort_definition_id in (@eventId)
          )
          SELECT
            e.cohort_definition_id as event_id,
            t.cohort_definition_id as target_id,
            e.cohort_start_date as event_start_date,
            e.cohort_end_date as event_end_date,
            t.cohort_start_date as target_start_date,
            t.cohort_end_date as target_end_date,
            t.subject_id,
            DATEDIFF(day, t.cohort_start_date, e.cohort_start_date) as diff
          FROM (
            SELECT *
            FROM @write_schema.@cohort_table
            WHERE cohort_definition_id = @targetId
          ) t
          INNER JOIN e ON
            e.subject_id = t.subject_id AND
            e.cohort_start_date >= t.cohort_start_date AND
            e.cohort_start_date <= t.cohort_end_date;"


  renderedSql <- SqlRender::render(
    sql = sql,
    write_schema = workDatabaseSchema,
    cohort_table = cohortTable,
    targetId = targetId,
    eventId = eventId
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  current_cohorts <- DatabaseConnector::querySql(connection = con, sql = renderedSql)

  names(current_cohorts) <- tolower(names(current_cohorts))
  current_cohorts <- data.table::as.data.table(current_cohorts)

  return(current_cohorts)
}


getTreatmentHistory <- function(con,
                                workDatabaseSchema,
                                cohortTable,
                                targetId,
                                eventId,
                                outputFolder) {

  tik <- Sys.time()

  # Collect cohorts data
  current_cohorts <- collectCohorts(con = con,
                                    workDatabaseSchema = workDatabaseSchema,
                                    cohortTable = cohortTable,
                                    targetId = targetId,
                                    eventId = eventId)

  # # Job log
  # tok <- Sys.time()
  # tdif <- tok - tik
  # tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  # cli::cat_line("\nTreatment History built at: ", crayon::green(tok))
  # cli::cat_line("Treatment History build took: ", crayon::green(tok_format))

  ## If current_cohorts object is empty, skip export
  if (nrow(current_cohorts) == 0) {

    return(NA)
  }

  # # Export
  # save_name <- paste("th", targetId,  sep = "_")
  # save_path <- fs::path(outputFolder, save_name, ext = "csv")
  # readr::write_csv(x = current_cohorts, file = save_path)

  # # Job log
  # cli::cat_line()
  # cli::cat_bullet("Saved file ", crayon::green(basename(save_path)), " to:", crayon::cyan(outputFolder), bullet = "info", bullet_col = "blue")
  # cli::cat_line()

  return(current_cohorts)
}


prepTte <- function(con,
                    th,
                    strata = NULL,
                    workDatabaseSchema,
                    cohortTable,
                    targetCohort) {

  # Job log
  cli::cat_line(crayon::blue("Extracting Survival Table..."))

  # Get target cohort id and name
  #targetCohortIds <- targetCohort$id
  #targetCohortNames <- targetCohort$name

  targetCohortIds <- targetCohort

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

  # # Loop through target cohort ids
  # for (i in seq_along(targetCohortIds)) {

    tte <- targetTbl %>%
      # filter to cohort id
      dplyr::filter(cohort_definition_id == targetCohortIds) %>%
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

  #}

  return(survival_info)
}



calculateStatisticsContinuous <- function(df,
                                          database,
                                          dateScale = c("default", "all")) {


  ## Days only
  if(dateScale == "default") {

  summaryStatistics <- df %>%
    group_by(event_id, target_id) %>%
    dplyr::summarise(
      p10 = quantile(diff, 0.1),
      p25 = quantile(diff, 0.25),
      median = median(diff),
      p75 = quantile(diff, 0.75),
      p90 = quantile(diff, 0.9),
      iqr = IQR(diff),
      mean = mean(diff),
      sd = sd(diff),
      min = min(diff),
      max = max(diff),
      n = n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dateScale = "days")

  }

  ## Days, weeks, months and years
  if(dateScale == "all") {

    ## Convert days to weeks, months and years
    df <- df %>% dplyr::mutate(diff_weeks = diff/7,
                               diff_months = diff/30,
                               diff_years = diff/365)

    ## Days
    summaryStatistics <- df %>%
      group_by(event_id, target_id) %>%
      dplyr::summarise(
        p10 = quantile(diff, 0.1),
        p25 = quantile(diff, 0.25),
        median = median(diff),
        p75 = quantile(diff, 0.75),
        p90 = quantile(diff, 0.9),
        iqr = IQR(diff),
        mean = mean(diff),
        sd = sd(diff),
        min = min(diff),
        max = max(diff),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dateScale = "days")

    ## Weeks
    summaryStatistics_weeks <- df %>%
      group_by(event_id, target_id) %>%
      dplyr::summarise(
        p10 = quantile(diff_weeks, 0.1),
        p25 = quantile(diff_weeks, 0.25),
        median = median(diff_weeks),
        p75 = quantile(diff_weeks, 0.75),
        p90 = quantile(diff_weeks, 0.9),
        iqr = IQR(diff_weeks),
        mean = mean(diff_weeks),
        sd = sd(diff_weeks),
        min = min(diff_weeks),
        max = max(diff_weeks),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dateScale = "weeks")

    ## Months
    summaryStatistics_months <- df %>%
      group_by(event_id, target_id) %>%
      dplyr::summarise(
        p10 = quantile(diff_months, 0.1),
        p25 = quantile(diff_months, 0.25),
        median = median(diff_months),
        p75 = quantile(diff_months, 0.75),
        p90 = quantile(diff_months, 0.9),
        iqr = IQR(diff_months),
        mean = mean(diff_months),
        sd = sd(diff_months),
        min = min(diff_months),
        max = max(diff_months),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dateScale = "months")

    ## Years
    summaryStatistics_years <- df %>%
      group_by(event_id, target_id) %>%
      dplyr::summarise(
        p10 = quantile(diff_years, 0.1),
        p25 = quantile(diff_years, 0.25),
        median = median(diff_years),
        p75 = quantile(diff_years, 0.75),
        p90 = quantile(diff_years, 0.9),
        iqr = IQR(diff_years),
        mean = mean(diff_years),
        sd = sd(diff_years),
        min = min(diff_years),
        max = max(diff_years),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(dateScale = "years")


    ## Bind all data frames together
    summaryStatistics <- rbind(summaryStatistics_years, summaryStatistics_months, summaryStatistics_weeks, summaryStatistics)
  }

  ## Add database name
  summaryStatistics <- summaryStatistics %>% dplyr::mutate(database = database)

  return(summaryStatistics)
}


## Main function -----------------------

executeTimeToEventSurvival <- function(con,
                                       executionSettings,
                                       analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings$tte$outputFolder) %>%
    fs::dir_create()

  targetCohorts <- analysisSettings$tte$cohorts$targetCohorts
  eventCohorts <- analysisSettings$tte$cohorts$eventCohorts

  # # Job log
  # cli::cat_boxx(crayon::magenta("Building Treatment History"))
  # cli::cat_line()
  # tik <- Sys.time()

  # Loop through target cohort ids
  for (i in seq_along(targetCohorts$id)) {

    # Target & event cohort ids
    targetId <- targetCohorts$id[i]
    eventId <- eventCohorts$id

    # Job log
    cli::cat_rule()
    txt1 <- paste0(targetCohorts$name[i], " (id:", targetCohorts$id[i], ")")
    cli::cat_bullet(crayon::green("Target Cohort: "), txt1, bullet = "pointer", bullet_col = "yellow")
    txt2 <- paste0(eventCohorts$name, " (id:", eventCohorts$id, ")", collapse = ", ")
    cli::cat_bullet(crayon::green("Event Cohorts: "), txt2, bullet = "pointer", bullet_col = "yellow")
    cli::cat_line()


    # Run treatment history
    current_cohorts <- getTreatmentHistory(con = con,
                                          workDatabaseSchema = workDatabaseSchema,
                                          cohortTable = cohortTable,
                                          targetId = targetId,
                                          eventId = eventId,
                                          outputFolder = outputFolder)


    # Warning if no data are returned from function above.
    # The data frame is empty if 1) there is no data for the target cohort or 2) there are no patients with the event cohort.
    # If there is no data, the loop continues with the next target cohort id.
    if (nrow(current_cohorts) == 0) {
      cli::cat_bullet("No data returned for target cohort id: ", crayon::red(targetId), ". Function will continue with the next cohort id.",
                      bullet = "info", bullet_col = "blue")
      next
    }


    # Get time to event data frame
    tteDat <- prepTte(con = con,
                      th = current_cohorts,
                      workDatabaseSchema = workDatabaseSchema,
                      cohortTable = cohortTable,
                      targetCohort = targetId)

  }

  # # Job log
  # tok <- Sys.time()
  # tdif <- tok - tik
  # tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  # cli::cat_line()
  # cli::cat_bullet("Execution took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  invisible(current_cohorts)
}




executeTimeToEvent <- function(con,
                               executionSettings,
                               analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings$tte$outputFolder) %>%
    fs::dir_create()

  targetCohorts <- analysisSettings$tte$cohorts$targetCohorts
  eventCohorts <- analysisSettings$tte$cohorts$eventCohorts

  # Job log
  cli::cat_boxx(crayon::magenta("Calculating Time To Event data"))
  cli::cat_line()
  tik <- Sys.time()

  # Loop through target cohort ids
  for (i in seq_along(targetCohorts$id)) {

    # Target & event cohort ids
    targetId <- targetCohorts$id[i]
    eventId <- eventCohorts$id

    # Job log
    cli::cat_rule()
    txt1 <- paste0(targetCohorts$name[i], " (id:", targetCohorts$id[i], ")")
    cli::cat_bullet(crayon::green("Target Cohort: "), txt1, bullet = "pointer", bullet_col = "yellow")
    txt2 <- paste0(eventCohorts$name, " (id:", eventCohorts$id, ")", collapse = ", ")
    cli::cat_bullet(crayon::green("Event Cohorts: "), txt2, bullet = "pointer", bullet_col = "yellow")
    cli::cat_line()


    # Run treatment history
    current_cohorts <- collectCohorts2(con = con,
                                       workDatabaseSchema = workDatabaseSchema,
                                       cohortTable = cohortTable,
                                       targetId = targetId,
                                       eventId = eventId)


    # Warning if no data are returned from function above.
    # The data frame is empty if 1) there is no data for the target cohort or 2) there are no patients with the event cohort.
    # If there is no data, the loop continues with the next target cohort id.
    if (nrow(current_cohorts) == 0) {
      cli::cat_bullet("No data returned for target cohort id: ", crayon::red(targetId), ". Function will continue with the next cohort id.",
                      bullet = "info", bullet_col = "blue")
      next
    }

    # Calculate summary statistics for continuous variable
    summaryStatistics <- calculateStatisticsContinuous(df = current_cohorts,
                                                       database = executionSettings$databaseName,
                                                       dateScale = "all")

    # Export object
    verboseSave(object = summaryStatistics,
                saveName = paste0("tteStatistics_", targetId),
                saveLocation = outputFolder)

   }


  # Bind and save files
  bindFiles(
    inputPath = outputFolder,
    outputPath = outputFolder,
    filename = "tteStats",
    pattern = "tteStatistics"
  )

  # Job log
  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line()
  cli::cat_bullet("Execution took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  invisible(summaryStatistics)
}
