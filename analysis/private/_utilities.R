# A. File Info -----------------------

# Task: Execution Settings


# B. Functions ------------------------

# Retrieve cohort names from JSON files in the `cohortToCreate` folder
getCohortManifest <- function(inputPath = here::here("cohortsToCreate")) {

  # Get cohort JSON file paths
  cohortFiles <- fs::dir_ls(inputPath, recurse = TRUE, type = "file", glob = "*.json")

  # Get cohort names out of JSON file name
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()

  # Get cohort type out of folder name
  cohortType <- fs::path_dir(cohortFiles) %>%
    basename() %>%
    gsub(".*_", "", .)

  # Add hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  # Return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cohortFiles %>% as.character()
  ) %>%
    dplyr::mutate(id = dplyr::row_number(), .before = 1)

  return(tb)
}


# This function is specific to run in Bayer's OMOP database structure (Snowflake)
startSnowflakeSession <- function(con, executionSettings) {

  sql <- "
  ALTER SESSION SET JDBC_QUERY_RESULT_FORMAT='JSON';
    USE ROLE @user_role;
    USE SECONDARY ROLES ALL;
    USE DATABASE @write_database;
    USE SCHEMA @write_schema;
  "
  crd <- stringr::str_split_1(string = executionSettings$workDatabaseSchema, pattern = "\\.")

  sessionSql <- SqlRender::render(
    sql = sql,
    user_role = executionSettings$role,
    write_database = crd[1],
    write_schema = crd[2]
  ) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, sql = sessionSql)

  cli::cat_line("Setting up Snowflake session")

  invisible(sessionSql)
}


# Convert analysis settings yaml files (exported by the analysisSettings.R script) to R lists
readSettingsFile <- function(settingsFile) {

  tt <- yaml::read_yaml(file = settingsFile)

  # Convert cohorts into data frames
  for (i in seq_along(tt[[1]][[1]])) {
    tt[[1]][[1]][[i]] <- listToTibble(tt[[1]][[1]][[i]])
  }

  # Convert unnamed lists into data frames
  ss <- seq_along(tt[[1]])

  for (j in ss[-1]) {
    check <- is.list(tt[[1]][[j]]) && is.null(names(tt[[1]][[j]]))
    if (check) {
      tt[[1]][[j]] <- listToTibble(tt[[1]][[j]])
    } else {
      next
    }
  }

  return(tt)
}


listToTibble <- function(ll) {

  df <- do.call(rbind.data.frame, ll) |>
    tibble::as_tibble()

  return(df)
}


## Function to save R objects as csv files
verboseSave <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)

  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:", crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(savePath)
}


## Function to save R objects as rds files
verboseSaveRds <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "rds")
  readr::write_rds(object, file = savePath)

  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:", crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(savePath)
}


# Zip study's results i.e. all files and folders under the `results` folder
zipResults <- function(database) {

  resultsPath <- here::here("results", database)

  # Zip "report" folder
  files2zip <- dir(resultsPath, full.names = TRUE, recursive = TRUE)
  files2zip <- files2zip[!grepl(".rds", files2zip)] # Exclude rds files

  if (length(database) >1) {

    zipName <- 'reportFiles'
  }
  else {

    zipName <- paste0('reportFiles_', database)
  }

  utils::zip(zipfile = zipName, files = files2zip)

  cli::cat_bullet("Study results have been zipped and saved to:",
                  crayon::cyan(here::here(paste0(zipName, ".zip"))),bullet = "info", bullet_col = "blue")
}


# Create data frame to run in purrr::map functions (three inputs)
createGrid <- function(cohortKey, timeA, timeB) {

  combos <- tidyr::expand_grid(cohortKey, timeA)

  repNo <- (nrow(cohortKey) * length(timeA))/length(timeB)

  combosAll <- combos %>%
    dplyr::mutate(timeB = rep(timeB, repNo))

  return(combosAll)
}


# Create data frame to run in purrr::map functions (four inputs)
createGrid2 <- function(cohortKey, covariateKey, timeA, timeB) {

  names(cohortKey) <- c("cohort_name", "cohort_id")
  names(covariateKey) <- c("covariate_name", "covariate_id")

  combos <- tidyr::expand_grid(cohortKey, covariateKey, timeA)

  repNo <- nrow(cohortKey) * nrow(covariateKey)

  combosAll <- combos %>%
    dplyr::mutate(timeB = rep(timeB, repNo))

  return(combosAll)
}


# Bind csv files together and export in specific location
bindFiles <- function(inputPath,
                      outputPath,
                      filename,
                      database = NULL,
                      pattern = NULL)  {


  # List all csv files in folder
  filepath <- list.files(inputPath, full.names = TRUE, pattern = pattern, recursive = TRUE)

  # Read all csv files and save in list
  listed_files <- lapply(filepath, readr::read_csv, show_col_types = FALSE)

  # Bind all data frames of list
  binded_df <- dplyr::bind_rows(listed_files)

  # Save output
  readr::write_csv(
    x = binded_df,
    file = file.path(here::here(outputPath, paste0(filename, ".csv"))),
    append = FALSE
  )

  # Delete individual files
  file.remove(filepath)

}


# Function that creates KM plots out of survfit rds files
createKMplots <- function(database) {

  ## Set variables
  appDataPath <- here::here("results", database)
  resultsPath <- here::here("results")

  ### Create a data frame of all permutations of paths
  allPaths <- tidyr::expand_grid(database, "06_tte") %>%
    dplyr::mutate(fullPath = fs::path(resultsPath, database, "06_tte"))

  ## List of files in "06_tte" folder
  listOftteFiles <- list.files(allPaths$fullPath[1], pattern = "tteSurvFit", recursive = FALSE, full.names	= TRUE)

  ## Create output folder
  outputFolder <- here::here(appDataPath, "ttePlots")
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

    ## Create KM plot
    tteSurvFit |>
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

  ## Bind all list objects together
  pickerListFinal <- do.call(rbind, pickerList)

  ## Export picker list
  readr::write_csv(pickerListFinal, file = fs::path(outputFolder, "ttePickers.csv"))

  ## Job log
  cli::cat_bullet(paste0("KM plots have been created and saved in: ", crayon::green(outputFolder)), bullet = "info", bullet_col = "blue")


  invisible(pickerListFinal)
}


# Mask low counts. Columns: n and/or pct. Default count(n) is 5 (inclusive)
maskLowCount <- function(df, countLimit = 5L, countOnly = FALSE) {

  if (countOnly) {

    dfLow <- df %>%
      dplyr::mutate(
        n = dplyr::if_else(n <= countLimit, -5, n, -5)
      )

  } else {

    dfLow <- df %>%
      dplyr::mutate(
        pct = dplyr::if_else(n <= countLimit, -5, pct, -5),
        n = dplyr::if_else(n <= countLimit, -5, n, -5)
      )

  }

  return(dfLow)
}


# Mask low counts. Columns: entries and subjects. Default count is 5 (inclusive)
maskLowCount2 <- function(df, countLimit = 5L) {

  dfLow <- df %>%
    dplyr::mutate(
      entries = dplyr::if_else(entries <= countLimit, -5, pct, -5),
      subjects = dplyr::if_else(subjects <= countLimit, -5, n, -5)
    )

  return(dfLow)
}

