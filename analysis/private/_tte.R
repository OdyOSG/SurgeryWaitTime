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


# Function that determines 1) censored and non-censored patients, 2) duration of right-censored patients and 3) fits the patient data frame
# Note that the duration for non-censored has already been calculated in the SQL code
prepTte <- function(df) {


    ## Determine censored and non-censored patients, and duration of right-censored patients
    tte <- df %>%
      dplyr::mutate(diff = dplyr::if_else(is.na(event_start_date),
                                          as.double(difftime(target_end_date, target_start_date, units = "days")), diff, diff),
                    status = dplyr::if_else(is.na(event_start_date), 0, 1, 0)) %>%
      dplyr::select(-c(rank, subject_id))


    ## Fit patient data frame
    survFit2 <- ggsurvfit::survfit2(
      survival::Surv(time = diff, event = status, type = "right") ~ eventName,
      data = tte
    )

  return(survFit2)
}


# Calculate continuous variable statistics
# Note that this function is grouping the data frame by columns 'event_id' and 'target_id'
# Specific to this study for now but we can generalize later
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


# Function to abbreviate event cohort names for tidy display in KM plots
# Note that this function is specific to this study
abbreviateEventNames <- function(df){

  df <- df %>%
    dplyr::mutate(eventName = dplyr::case_when(
      event_id == 3 ~ "APAP",
      event_id == 4 ~ "HCTZ",
      event_id == 5 ~ "proc1",
      event_id == 6 ~ "proc2",
      TRUE ~ NA
    ))

  return(df)
}


## Main functions -----------------------

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


    # Collect patient data
    current_cohorts <- collectCohorts(con = con,
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
      cli::cat_line()

      next
    }


    # Abbreviate event names for KM plots
    current_cohorts<- abbreviateEventNames(df = current_cohorts)

    # Get time to event data
    tteSurvFit <- prepTte(df = current_cohorts)

    # Export object
    verboseSaveRds(object = tteSurvFit,
                   saveName = paste0("tteSurvFit_", targetId),
                   saveLocation = outputFolder)

  }

  # Job log
  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_line()
  cli::cat_bullet("Execution took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")


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


    # Collect patient data
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
