# A. Meta Info -----------------------

# Title: Concept Prevalence
# Description: These internal function run prevalence of concepts using Feature Extraction


# B. Helpers -----------------

silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings) {

  cli::cat_bullet("Getting Covariates from database...",
                  bullet = "info", bullet_col = "blue")

  tik <- Sys.time()

  # Get covariate data
  quietCov <- purrr::quietly(FeatureExtraction::getDbCovariateData)
  cov <- quietCov(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covariateSettings = covSettings,
    aggregated = TRUE
  )$result

  tok <- Sys.time()

  cli::cat_bullet("Covariates built at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Covariate build took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")

  return(cov)
}


verboseSave <- function(object, saveName, saveLocation) {

  savePath <- fs::path(saveLocation, saveName, ext = "csv")
  readr::write_csv(object, file = savePath)
  cli::cat_line()
  cli::cat_bullet("Saved file ", crayon::green(basename(savePath)), " to:",
                  bullet = "info", bullet_col = "blue")
  cli::cat_bullet(crayon::cyan(saveLocation), bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()

  invisible(savePath)
}


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

  ## Save output
  readr::write_csv(
    x = binded_df,
    file = file.path(here::here(outputPath, paste0(filename, ".csv"))),
    append = FALSE
  )

  ## Delete individual files
  file.remove(filepath)

}


# C. Domain FE -------------------------

getDrugsFE <- function(con,
                       cohortDatabaseSchema,
                       cohortTable,
                       cdmDatabaseSchema,
                       cohortId,
                       type = c("postIndex", "baseline"),
                       timeA,
                       timeB,
                       outputFolder) {

  cli::cat_rule("Build Drug Covariates")

  # Create Drug settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDrugGroupEraLongTerm = TRUE,
    excludedCovariateConceptIds = c(21600001, 21600959, 21601237, # Remove ATC 1st class
                                    21601907, 21602359, 21602681,
                                    21602795, 21601386, 21603931,
                                    21604180, 21604847, 21605007,
                                    21603550, 21605212),
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  drugTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_", abs(timeB)),
      database = executionSettings$databaseName
      )

  # Output file name
  saveName <- paste0("drugs_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Save results
  verboseSave(
    object = drugTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(drugTbl)
}


getConditionsFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            type = c("postIndex", "baseline"),
                            timeA,
                            timeB,
                            outputFolder) {

  cli::cat_rule("Build Condition Covariates")

  # Create Condition settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  condTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("conditions_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Save results
  verboseSave(
    object = condTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(condTbl)
}


getProceduresFE <- function(con,
                                cohortDatabaseSchema,
                                cohortTable,
                                cdmDatabaseSchema,
                                cohortId,
                                type = c("postIndex", "baseline"),
                                timeA,
                                timeB,
                                outputFolder) {

  cli::cat_rule("Build Procedure Covariates")

  # Create Procedure settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useProcedureOccurrenceLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  procTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("procedures_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Save results
  verboseSave(
    object = procTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(procTbl)
}


getVisitsFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            type = c("postIndex", "baseline"),
                            timeA,
                            timeB,
                            outputFolder) {

  cli::cat_rule("Build Visit Covariates")

  # Create Visit settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useVisitCountLongTerm = TRUE,
    useVisitConceptCountLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  visitTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName,
      n = countValue,
      max = maxValue,
      min = minValue,
      mean = averageValue,
      median = medianValue,
      p10 = p10Value,
      p25 = p25Value,
      p75 = p75Value,
      p90 = p90Value,
      sd = standardDeviation
    ) %>%
    dplyr::select(-covariateId) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("visits_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Save results
  verboseSave(
    object = visitTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(visitTbl)
}


getObservationsFE <- function(con,
                              cohortDatabaseSchema,
                              cohortTable,
                              cdmDatabaseSchema,
                              cohortId,
                              type = c("postIndex", "baseline"),
                              timeA,
                              timeB,
                              outputFolder) {

  cli::cat_rule("Build Observation Covariates")

  # Create Observation settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useObservationLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  obsTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("observations_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Save results
  verboseSave(
    object = obsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(obsTbl)
}


getCohortFE <- function(con,
                        cohortDatabaseSchema,
                        cohortTable,
                        cdmDatabaseSchema,
                        cohortId,
                        analysisSettings,
                        covId,
                        type = c("postIndex", "baseline"),
                        timeA,
                        timeB,
                        outputFolder) {

  cli::cat_rule("Build Cohort Covariates")

  # Create Covariate tibble for FE
  covariateCohorts <- as_tibble(analysisSettings[[1]]$cohorts$covariateCohorts)

  covariateCohorts <- covariateCohorts %>%
    dplyr::filter(id == covId) %>%
    dplyr::rename(
      cohortId = id,
      cohortName = name
    )


  # Create Cohort settings
  covSettings <- FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = 999L,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts = covariateCohorts,
    valueType = "binary",
    startDay = timeA,
    endDay = timeB
  )

  # Run FE
  cov <- silentCovariates(
    con = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covSettings = covSettings
    )

  # Format
  cohortTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_", abs(timeB)),
      database = executionSettings$databaseName,
      cohortDefinitionId = as.integer(cohortDefinitionId),
      covariateId = as.integer(covariateId),
      n = as.integer(n),
      pct = as.double(pct),
      analysisId = as.integer(analysisId),
      conceptId = as.integer(conceptId)
                  )

  # Output file name
  saveName <- paste0("cohort_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB), "_", covId)

  ## Save file only if it has rows
  if (nrow(cohortTbl) >0 )
    {

    # Save results
    verboseSave(
      object = cohortTbl,
      saveName = saveName,
      saveLocation = outputFolder
    )

  }

  invisible(cohortTbl)
}


getDemographicsFE <- function(con,
                              cohortDatabaseSchema,
                              cohortTable,
                              cdmDatabaseSchema,
                              cohortId,
                              outputFolder) {

  cli::cat_rule("Build Demographic Covariates")

  # Create Demographic settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE,
    useDemographicsIndexYear = TRUE
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  demoTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(-covariateId) %>%
    dplyr::collect() %>%
    dplyr::mutate(database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("demographics_baseline_", cohortId)

  # Save results
  verboseSave(
    object = demoTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(demoTbl)
}


getContinuousFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            outputFolder) {

  cli::cat_rule("Build Continuous Covariates")

  # Create Continuous settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useCharlsonIndex = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # Format
  ctsTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName,
      n = countValue,
      max = maxValue,
      min = minValue,
      mean = averageValue,
      median = medianValue,
      p10 = p10Value,
      p25 = p25Value,
      p75 = p75Value,
      p90 = p90Value,
      sd = standardDeviation
    ) %>%
    dplyr::select(-covariateId) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("continuous_baseline_", cohortId)

  # Save results
  verboseSave(
    object = ctsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(ctsTbl)
}


# D. Execute ----------------------

executeConceptCharacterization <- function(con,
                                           type = c("postIndex", "baseline"),
                                           runDrugs = FALSE,
                                           runConditions = FALSE,
                                           runVisits = FALSE,
                                           runDemographics = FALSE,
                                           runContinuous = FALSE,
                                           runProcedures = FALSE,
                                           runObservations = FALSE,
                                           runCohorts = FALSE,
                                           executionSettings,
                                           analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  # Target and covariate cohort ids
  cohortKey <- analysisSettings[[1]]$cohorts$targetCohorts
  covariateKey <- analysisSettings[[1]]$cohorts$covariateCohorts

  # Time windows
  timeA <- analysisSettings[[1]]$timeWindows$startDay
  timeB <- analysisSettings[[1]]$timeWindows$endDay


  # Job Log
  typeAnalysis <- as.data.frame(x = type) %>%
    dplyr::mutate(fullName =
                    dplyr::case_when(type == "postIndex" ~ "Post-index",
                                     type == "baseline" ~ "Baseline",
                                     TRUE ~ type),
                  shortName =
                    dplyr::case_when(type == "postIndex" ~ "Post",
                                     type == "baseline" ~ "Base",
                                     TRUE ~ type)
                  )

  cohortId <- cohortKey$targetId

  cli::cat_boxx(paste0("Building ", typeAnalysis$fullName, " Covariates"))
  cli::cat_bullet(paste0("Using ", typeAnalysis$fullName,  " Window: [", crayon::green(timeA), ", ", crayon::green(timeB), "]"),
                  bullet = "info", bullet_col = "blue")
  cat_cohortId <- paste(cohortId, collapse = ", ")
  cli::cat_bullet(paste0("Building ", typeAnalysis$fullName, " covariates for cohort ids:\n   ", crayon::green(cat_cohortId)),
                  bullet = "info", bullet_col = "blue")
  cli::cat_line()



  tik <- Sys.time()

  if (type == "baseline") {

      # Run Baseline Demographics
      if (runDemographics == TRUE) {

        # Calculate for each cohort
        purrr::pmap_dfr(cohortKey,
                        ~ getDemographicsFE(con = con,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            cohortTable = cohortTable,
                                            cohortDatabaseSchema = workDatabaseSchema,
                                            cohortId = ..1,
                                            outputFolder = outputFolder)
        )

        # Bind files
        bindFiles(
          inputPath = outputFolder,
          outputPath = outputFolder,
          filename = "demoBase",
          pattern = "demographics_baseline"
        )

     }

    # Run Baseline Continuous
    if (runContinuous == TRUE) {

      # Calculate for each cohort
      purrr::pmap_dfr(cohortKey,
                      ~ getContinuousFE(con = con,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDatabaseSchema = workDatabaseSchema,
                                        cohortId = ..1,
                                        outputFolder = outputFolder)
      )

      # Bind files
      bindFiles(
        inputPath = outputFolder,
        outputPath = outputFolder,
        filename = "contBase",
        pattern = "continuous_baseline"
      )

    }
  }

  # Create grid df for execution
  drugGrid <- createGrid(
    cohortKey = cohortKey,
    timeA = timeA,
    timeB = timeB
    )


  # Run Drugs Covariates
  if (runDrugs == TRUE) {

    # Calculate for each cohort and time window
    purrr::pmap_dfr(drugGrid,
                    ~ getDrugsFE(con = con,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortTable = cohortTable,
                                 cohortDatabaseSchema = workDatabaseSchema,
                                 type = type,
                                 cohortId = ..1,
                                 timeA = ..3,
                                 timeB = ..4,
                                 outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("drugs", typeAnalysis$shortName),
      pattern = paste0("drugs_", typeAnalysis$type)
    )

  }

  # Run Conditions Covariates
  if (runConditions == TRUE) {

    # Calculate for each cohort and time window
    purrr::pmap_dfr(grid,
                    ~ getConditionsFE(con = con,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = workDatabaseSchema,
                                      type = type,
                                      cohortId = ..1,
                                      timeA = ..3,
                                      timeB = ..4,
                                      outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cond", typeAnalysis$shortName),
      pattern = paste0("conditions_", typeAnalysis$type)
    )

  }

  # Run Visits Covariates
  if (runVisits == TRUE) {

    # Calculate for each cohort and time window
    purrr::pmap_dfr(grid,
                    ~ getVisitsFE(con = con,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortDatabaseSchema = workDatabaseSchema,
                                  type = type,
                                  cohortId = ..1,
                                  timeA = ..3,
                                  timeB = ..4,
                                  outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("visit", typeAnalysis$shortName),
      pattern = paste0("visits_", typeAnalysis$type)
    )

  }

  # Run Procedures Covariates
  if (runProcedures == TRUE) {

    # Calculate for each cohort and time window
    purrr::pmap_dfr(grid,
                    ~ getProceduresFE(con = con,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = workDatabaseSchema,
                                      type = type,
                                      cohortId = ..1,
                                      timeA = ..3,
                                      timeB = ..4,
                                      outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("proc", typeAnalysis$shortName),
      pattern = paste0("procedures_", typeAnalysis$type)
    )

  }

  # Run Observations Covariates
  if (runObservations == TRUE) {

    # Calculate for each cohort and time window
    purrr::pmap_dfr(grid,
                    ~ getObservationsFE(con = con,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDatabaseSchema = workDatabaseSchema,
                                        type = type,
                                        cohortId = ..1,
                                        timeA = ..3,
                                        timeB = ..4,
                                        outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("obs", typeAnalysis$shortName),
      pattern = paste0("observations", typeAnalysis$type)
    )

  }


  # Create grid df for execution
  cohortGrid <- createGrid2(
    cohortKey = cohortKey,
    covariateKey = covariateKey,
    timeA = timeA,
    timeB = timeB
    )

  # Run Cohort Covariates
  if (runCohorts == TRUE) {

    # Calculate for each cohort, covariate and time window
    purrr::pmap_dfr(cohortGrid,
                    ~ getCohortFE(con = con,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortDatabaseSchema = workDatabaseSchema,
                                  analysisSettings = analysisSettings,
                                  type = type,
                                  covId = ..4,
                                  cohortId = ..1,
                                  timeA = ..5,
                                  timeB = ..6,
                                  outputFolder = outputFolder)
    )

    # Bind files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cohort", typeAnalysis$shortName),
      pattern = paste0("cohort_", typeAnalysis$type)
    )

  }


  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok),
                  bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format),
                  bullet = "info", bullet_col = "blue")

  invisible(tok)
}
