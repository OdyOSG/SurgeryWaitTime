# A. File Info -----------------------

# Task: Time To Event


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
    if(nrow(tte) > 100) {
          ## Fit patient data frame
    survFit2 <- ggsurvfit::survfit2(
      survival::Surv(time = diff, event = status, type = "right") ~ eventName,
      data = tte
    )
  return(survFit2)
    }
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
      event_id == 9 ~ "SWT breast surgery outcome",
      event_id == 10 ~ "SWT colorectal surgery outcome",
      event_id == 11 ~ "SWT esophagus surgery outcome",
      event_id == 12 ~ "SWT lung surgery outcome",
      TRUE ~ NA
    ))

  return(df)
}


# Function that creates KM plots out of survfit rds files
createKMplots <- function(database) {

  ## Set variables
  appDataPath <- here::here("results", database)
  resultsPath <- here::here("results")

  ## Create a data frame of all permutations of paths
  allPaths <- tidyr::expand_grid(database, "06_tte") %>%
    dplyr::mutate(fullPath = fs::path(resultsPath, database, "06_tte"))

  ## List of files in "06_tte" folder
  listOftteFiles <- list.files(allPaths$fullPath[1], pattern = "tteSurvFit", recursive = FALSE, full.names	= TRUE)

  ## Create output folder
  outputFolder <- here::here(appDataPath, "06_ttePlots")
  outputFolder %>% fs::dir_create()

  ## Create list to save cohort and database values to determine picker values
  pickerList <- vector("list", length = length(listOftteFiles))


  ## Loop through rds files to create png files for KM plots
  for (i in 1:length(listOftteFiles)) {

    ## Read rds file (survfit object)
      tte <- readr::read_rds(listOftteFiles[i])
      tteSurvFit <- tte[["survFit"]]

      ## Number of colors should be equal to the number of unique strata values i.e. events (lines in KM plot)
      colors <- colorspace::rainbow_hcl(unique(length(tteSurvFit$strata)))
      if (!is.null(tteSurvFit)) {
        ## Create KM plot
        tteSurvFit %>%
          ggsurvfit::ggsurvfit(size = 1) +
          ggsurvfit::scale_ggsurvfit(x_scales = list(breaks = c(183, 365, 548, 730))) + # Breaks
          ggplot2::scale_color_manual(values = colors) +
          ggplot2::scale_fill_manual(values = colors) +
          ggsurvfit::add_risktable(risktable_stats = "{n.risk} ({cum.event})",
                                   risktable_height = 0.4,
                                   hjust = 0,
                                   size = 4, # Increase font size of risk table statistics
                                   theme =
                                     # Increase font size of risk table title and y-axis label
                                     list(
                                       ggsurvfit::theme_risktable_default(axis.text.y.size = 11,
                                                                          plot.title.size = 11),
                                       theme(plot.title = element_text(face = "bold"),
                                             plot.margin = unit(c(5.5, 50, 5.5, 5.5), "points"),
                                             axis.text.x = element_text(hjust = -5)
                                       )
                                     )) +
          labs(x = "Follow-up time, days")

        ## Add cohort and database values to picker list
        pickerList[[i]] <- data.frame(database = tte$database, cohortId = tte$cohortId, cohortName = tte$cohortName)

        ## Save plot
        ggplot2::ggsave(filename = here::here(outputFolder, paste0("tte_", tte$database, "_", tte$cohortId, ".png")),
                        width = 18, height = 14)
      }


  }

  ## Bind all list objects together
  pickerListFinal <- do.call(rbind, pickerList)

  ## Export picker list
  readr::write_csv(pickerListFinal, file = fs::path(outputFolder, "ttePickers.csv"))

  ## Job log
  cli::cat_bullet(paste0("KM plots have been created and saved in: ", crayon::green(outputFolder)), bullet = "info", bullet_col = "blue")


  invisible(pickerListFinal)
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
    if (nrow(current_cohorts) < 1 || nrow(current_cohorts %>% dplyr::filter(!is.na(event_id))) < 100) {
      cli::cat_bullet("No data returned for target cohort id: ", crayon::red(targetId), ". Function will continue with the next cohort id.",
                      bullet = "info", bullet_col = "blue")
      cli::cat_line()

      next
    }


    # Abbreviate event names for KM plots
    current_cohorts<- abbreviateEventNames(df = current_cohorts)

    # Get time to event data (list)
    tteSurvFit <- prepTte(df = current_cohorts)
    if(!is.null(tteSurvFit)) {
      # Add database and cohort to list to be exported
      tteList <- list(survFit = tteSurvFit, database = databaseId, cohortId = targetId, cohortName = targetCohorts$name[i])

      # Export object (list for KM plots)
      verboseSaveRds(object = tteList,
                     saveName = paste0("tteSurvFit_", targetId),
                     saveLocation = outputFolder)


      # Get time to event data (data frame)
      tteSurvDat <- ggsurvfit::tidy_survfit(tteSurvFit) %>%
        dplyr::select(time, n.risk, n.event, estimate, std.error, strata, conf.high, conf.low) %>%
        dplyr::mutate(database = databaseId,
                      targetCohort = targetId)

      # Export object (data frame for survival probabilities)
      verboseSave(object = tteSurvDat,
                  saveName = paste0("tteTables_", targetId),
                  saveLocation = outputFolder)
    }
  }

  # Bind and save csv files
  bindFiles(
    inputPath = outputFolder,
    outputPath = outputFolder,
    filename = "tteSurvTables",
    pattern = "tteTables"
  )

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
