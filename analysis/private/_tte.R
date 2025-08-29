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

  # Determine censored and non-censored patients, and duration of right-censored patients
  # NO hard censoring - preserve all follow-up data for flexibility
  tte <- df %>%
    dplyr::mutate(
      diff = dplyr::if_else(is.na(event_start_date),
                            as.double(difftime(target_end_date, target_start_date, units = "days")),
                            diff, diff),
      status = dplyr::if_else(is.na(event_start_date), 0, 1, 0)
    ) %>%
    dplyr::select(-c(rank, subject_id))

  if(nrow(tte) > 30) {
    # Fit patient data frame using all available follow-up data
    survFit2 <- ggsurvfit::survfit2(
      survival::Surv(time = diff, event = status, type = "right") ~ eventName,
      data = tte
    )
    return(survFit2)
  }
  return(NULL)
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

# Function to collect cohorts with patient demographics for stratified analysis
collectCohortsWithDemographics <- function(con, workDatabaseSchema, cohortTable, cdmDatabaseSchema, targetId, eventId) {

  # SQL query to collect patient data with demographics
  sql <- "
    WITH event_cohorts AS (
      SELECT *
      FROM @write_schema.@cohort_table
      WHERE cohort_definition_id in (@eventId)
    ),
    target_with_demo AS (
      SELECT
        t.cohort_definition_id,
        t.subject_id,
        t.cohort_start_date,
        t.cohort_end_date,
        p.gender_concept_id,
        p.race_concept_id,
        p.ethnicity_concept_id,
        YEAR(t.cohort_start_date) - p.year_of_birth AS age_at_index,
        c1.concept_name AS gender,
        c2.concept_name AS race,
        c3.concept_name AS ethnicity,
        -- Age groups
        CASE
          WHEN YEAR(t.cohort_start_date) - p.year_of_birth < 18 THEN 'Under 18'
          WHEN YEAR(t.cohort_start_date) - p.year_of_birth BETWEEN 18 AND 44 THEN '18-44'
          WHEN YEAR(t.cohort_start_date) - p.year_of_birth BETWEEN 45 AND 64 THEN '45-64'
          WHEN YEAR(t.cohort_start_date) - p.year_of_birth BETWEEN 65 AND 74 THEN '65-74'
          WHEN YEAR(t.cohort_start_date) - p.year_of_birth >= 75 THEN '75+'
          ELSE 'Unknown'
        END AS age_group,
        -- Extract facility type from care_site if available
        COALESCE(cs.care_site_name, 'Unknown') AS facility_type
      FROM @write_schema.@cohort_table t
      INNER JOIN @cdm_schema.person p ON p.person_id = t.subject_id
      LEFT JOIN @cdm_schema.concept c1 ON c1.concept_id = p.gender_concept_id
      LEFT JOIN @cdm_schema.concept c2 ON c2.concept_id = p.race_concept_id
      LEFT JOIN @cdm_schema.concept c3 ON c3.concept_id = p.ethnicity_concept_id
      LEFT JOIN @cdm_schema.care_site cs ON cs.care_site_id = p.care_site_id
      WHERE t.cohort_definition_id = @targetId
    )
    SELECT
      e.cohort_definition_id as event_id,
      t.cohort_definition_id as target_id,
      e.cohort_start_date as event_start_date,
      e.cohort_end_date as event_end_date,
      t.cohort_start_date as target_start_date,
      t.cohort_end_date as target_end_date,
      t.subject_id,
      t.gender,
      t.race,
      t.ethnicity,
      t.age_group,
      t.facility_type,
      t.age_at_index,
      -- Calculate time to event in days (preserve all follow-up data)
      CASE
        WHEN e.cohort_start_date IS NULL THEN
          DATEDIFF(day, t.cohort_start_date, t.cohort_end_date)
        ELSE
          DATEDIFF(day, t.cohort_start_date, e.cohort_start_date)
      END as time_to_event_days,
      -- Event indicator (1 = event occurred, 0 = censored)
      CASE
        WHEN e.cohort_start_date IS NULL THEN 0
        ELSE 1
      END as event_indicator
    FROM target_with_demo t
    LEFT JOIN event_cohorts e ON
      e.subject_id = t.subject_id AND
      e.cohort_start_date >= t.cohort_start_date AND
      e.cohort_start_date <= t.cohort_end_date"

  renderedSql <- SqlRender::render(
    sql = sql,
    write_schema = workDatabaseSchema,
    cdm_schema = cdmDatabaseSchema,
    cohort_table = cohortTable,
    targetId = targetId,
    eventId = paste(eventId, collapse = ",")
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  current_cohorts <- DatabaseConnector::querySql(connection = con, sql = renderedSql)
  names(current_cohorts) <- tolower(names(current_cohorts))

  return(data.table::as.data.table(current_cohorts))
}

# Function to determine cancer type from cohort name or ID
getCancerType <- function(cohortName, cohortId) {
  cancer_type <- case_when(
    str_detect(tolower(cohortName), "breast") ~ "breast",
    str_detect(tolower(cohortName), "colorectal") ~ "colorectal",
    str_detect(tolower(cohortName), "esophag") ~ "esophagus",
    str_detect(tolower(cohortName), "lung") ~ "lung",
    TRUE ~ "unknown"
  )
  return(cancer_type)
}

# Enhanced function to create stratified survival fits
prepTteStratified <- function(df, strata_var) {

  # Ensure we have the required columns
  required_cols <- c("time_to_event_days", "event_indicator", strata_var)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    cli::cat_bullet("Missing required columns: ", paste(missing_cols, collapse = ", "),
                    bullet = "cross", bullet_col = "red")
    return(NULL)
  }

  # Remove rows with missing strata values, but preserve all follow-up data
  df_clean <- df %>%
    filter(!is.na(.data[[strata_var]]), !is.na(time_to_event_days), !is.na(event_indicator)) %>%
    # Ensure minimum follow-up time (but no maximum - preserve all data)
    mutate(time_to_event_days = pmax(1, time_to_event_days))

  # Check if we have enough data for analysis
  if (nrow(df_clean) < 10) {
    cli::cat_bullet("Insufficient data for survival analysis (n < 10)",
                    bullet = "cross", bullet_col = "red")
    return(NULL)
  }

  # Create survival formula
  surv_formula <- as.formula(paste("Surv(time_to_event_days, event_indicator) ~", strata_var))

  # Fit survival model using all available follow-up data
  survFit <- try({
    survfit(surv_formula, data = df_clean)
  }, silent = TRUE)

  if (inherits(survFit, "try-error")) {
    cli::cat_bullet("Error fitting survival model", bullet = "cross", bullet_col = "red")
    return(NULL)
  }

  return(survFit)
}

# Enhanced function to create high-resolution KM plots with p-values
createEnhancedKMplot <- function(survfit_obj, title, strata_var, output_path, max_days = 365, width = 12, height = 8) {

  if (is.null(survfit_obj)) {
    return(NULL)
  }

  # Perform log-rank test for p-value
  pval <- NULL
  if (length(survfit_obj$strata) > 1) {
    pval_test <- try({
      survdiff(survfit_obj$call$formula, data = eval(survfit_obj$call$data))
    }, silent = TRUE)

    if (!inherits(pval_test, "try-error")) {
      pval <- pchisq(pval_test$chisq, length(pval_test$n) - 1, lower.tail = FALSE)
    }
  }

  # Create the plot
  p <- survfit_obj %>%
    ggsurvfit(size = 1.2, alpha = 0.8) +
    scale_ggsurvfit(
      x_scales = list(
        name = "Time (days)",
        breaks = seq(0, max_days, by = max(60, round(max_days/6))),
        limits = c(0, max_days)
      ),
      y_scales = list(
        name = "Survival Probability",
        breaks = seq(0, 1, by = 0.2),
        limits = c(0, 1)
      )
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_viridis_d(name = str_to_title(str_replace_all(strata_var, "_", " "))) +
    scale_fill_viridis_d(name = str_to_title(str_replace_all(strata_var, "_", " "))) +
    add_confidence_interval(alpha = 0.2) +
    add_risktable(
      risktable_stats = c("n.risk", "cum.event"),
      risktable_height = 0.35,
      theme = list(
        theme_risktable_default(
          axis.text.y.size = 10,
          plot.title.size = 11
        ),
        theme(
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 9)
        )
      )
    ) +
    labs(
      title = title,
      subtitle = if (!is.null(pval)) paste("Log-rank test p-value:", format.pval(pval, digits = 3)) else "",
      caption = paste("Display limited to", max_days, "days - Full follow-up data preserved")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )

  # Save plot with high resolution
  ggsave(
    filename = output_path,
    plot = p,
    width = width,
    height = height,
    dpi = 350,
    units = "in",
    bg = "white"
  )

  return(list(plot = p, pvalue = pval))
}


# Updated main function to create separate plots by cancer type and stratification
createKMplots <- function(database, con = NULL, executionSettings = NULL, max_display_days = 365) {

  cli::cat_boxx("Creating Enhanced KM Plots by Cancer Type")
  cli::cat_bullet("Display limited to ", max_display_days, " days (easily adjustable)",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet("All follow-up data preserved for flexibility",
                  bullet = "info", bullet_col = "blue")

  # Set up paths
  appDataPath <- here::here("results", database)
  resultsPath <- here::here("results")

  # List all survfit RDS files
  allPaths <- tidyr::expand_grid(database, "06_tte") %>%
    dplyr::mutate(fullPath = fs::path(resultsPath, database, "06_tte"))

  listOftteFiles <- list.files(allPaths$fullPath[1], pattern = "tteSurvFit",
                               recursive = FALSE, full.names = TRUE)

  if (length(listOftteFiles) == 0) {
    cli::cat_bullet("No survfit files found", bullet = "cross", bullet_col = "red")
    return(NULL)
  }

  # Create output folders for each cancer type
  cancer_types <- c("breast", "colorectal", "esophagus", "lung")
  outputFolders <- map(cancer_types, ~{
    folder <- here::here(appDataPath, paste0("06_ttePlots_", .x))
    fs::dir_create(folder)
    return(folder)
  })
  names(outputFolders) <- cancer_types

  # Stratification variables
  strata_vars <- c("gender", "age_group", "race", "ethnicity", "facility_type")

  # Process each survfit file
  results_summary <- tibble()

  for (i in seq_along(listOftteFiles)) {

    cli::cat_rule(paste("Processing file", i, "of", length(listOftteFiles)))

    # Read the RDS file
    tte_data <- readr::read_rds(listOftteFiles[i])

    # Extract metadata
    database_name <- tte_data$database
    cohort_id <- tte_data$cohortId
    cohort_name <- tte_data$cohortName

    # Determine cancer type
    cancer_type <- getCancerType(cohort_name, cohort_id)

    if (cancer_type == "unknown") {
      cli::cat_bullet("Skipping unknown cancer type for cohort: ", cohort_name,
                      bullet = "info", bullet_col = "blue")
      next
    }

    cli::cat_bullet("Processing ", cancer_type, " cancer cohort: ", cohort_name,
                    bullet = "pointer", bullet_col = "yellow")

    # Get the appropriate output folder
    output_folder <- outputFolders[[cancer_type]]

    # Re-read the original data with demographics
    if (!is.null(con) && !is.null(executionSettings)) {
      cohort_data <- collectCohortsWithDemographics(
        con = con,
        workDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTable = executionSettings$cohortTable,
        cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
        targetId = cohort_id,
        eventId = c(9, 10, 11, 12)  # All surgery outcomes
      )

      if (nrow(cohort_data) == 0) {
        cli::cat_bullet("No data available for cohort ", cohort_id,
                        bullet = "cross", bullet_col = "red")
        next
      }

      # Create stratified plots for each variable
      for (strata_var in strata_vars) {

        if (!strata_var %in% names(cohort_data)) {
          cli::cat_bullet("Skipping ", strata_var, " - variable not found",
                          bullet = "info", bullet_col = "blue")
          next
        }

        # Check if we have variation in the stratification variable
        unique_values <- cohort_data %>%
          pull(.data[[strata_var]]) %>%
          unique() %>%
          na.omit()

        if (length(unique_values) < 2) {
          cli::cat_bullet("Skipping ", strata_var, " - insufficient variation",
                          bullet = "info", bullet_col = "blue")
          next
        }

        # Create survival fit
        survfit_obj <- prepTteStratified(cohort_data, strata_var)

        if (is.null(survfit_obj)) {
          next
        }

        # Create plot title
        plot_title <- paste0(
          str_to_title(cancer_type), " Cancer: Time to First Treatment\n",
          "Stratified by ", str_to_title(str_replace_all(strata_var, "_", " "))
        )

        # Create output filename
        output_filename <- paste0(
          "km_", cancer_type, "_", strata_var, "_cohort_", cohort_id, ".png"
        )
        output_path <- file.path(output_folder, output_filename)

        # Create and save the plot
        plot_result <- createEnhancedKMplot(
          survfit_obj = survfit_obj,
          title = plot_title,
          strata_var = strata_var,
          output_path = output_path,
          max_days = max_display_days  # Pass the flexible display parameter
        )

        if (!is.null(plot_result)) {
          # Store results summary
          results_summary <- bind_rows(
            results_summary,
            tibble(
              cancer_type = cancer_type,
              cohort_id = cohort_id,
              cohort_name = cohort_name,
              strata_var = strata_var,
              p_value = plot_result$pvalue %||% NA_real_,
              n_patients = nrow(cohort_data),
              n_events = sum(cohort_data$event_indicator, na.rm = TRUE),
              output_file = output_filename
            )
          )

          cli::cat_bullet("Created plot: ", output_filename,
                          bullet = "tick", bullet_col = "green")
        }
      }
    } else {
      cli::cat_bullet("Skipping stratified plots - connection or settings missing",
                      bullet = "info", bullet_col = "blue")
    }
  }

  # Save results summary
  summary_file <- file.path(appDataPath, "km_plots_summary.csv")
  write_csv(results_summary, summary_file)

  cli::cat_bullet("Results summary saved to: ", summary_file,
                  bullet = "tick", bullet_col = "green")
  cli::cat_bullet("Enhanced KM plots created successfully!",
                  bullet = "tick", bullet_col = "green")

  return(results_summary)
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
      tteList <- list(survFit = tteSurvFit, database = databaseId, cohortId = targetId, cohortName = targetCohorts$