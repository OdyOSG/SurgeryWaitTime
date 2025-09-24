# A. File Info -----------------------

# Task: Concept Prevalence


# B. Functions -----------------

## Helper functions -----------------------

# Function that runs function FeatureExtraction::getDbCovariateData without console messages
silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings) {

  #Job log
  cli::cat_bullet("Getting Covariates from database...", bullet = "info", bullet_col = "blue")
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

  # Job log
  tok <- Sys.time()
  cli::cat_bullet("Covariates built at: ", crayon::red(tok), bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Covariate build took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  return(cov)
}



## Domain FE functions -------------------------

# Function that runs function FeatureExtraction::createCovariateSettings for drug eras (exlc. ATC 1st level classes)
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
    useDrugExposureLongTerm = TRUE,
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = drugTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(drugTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for condition eras
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
    useConditionOccurrenceLongTerm = TRUE,
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = condTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(condTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for condition eras
getConditionsCustomFE <- function(con,
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
    useConditionOccurrenceLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB,
    includedCovariateConceptIds = c(
      4044240,
      40491439,
      4006969,
      42873168,
      4162282,
      4197065,
      37017244,
      196724,
      3656669,
      37110295,
      43531585,
      313459,
      4306572,
      4247108,
      4317284,
      4170108,
      4063381,
      4154706,
      4028286,
      318459,
      317009,
      40488781,
      4098766,
      4323557,
      4055224,
      42538550,
      4340951,
      4341650,
      35621839,
      4342777,
      4144765,
      4296301,
      36675115,
      4245975,
      3655102,
      444421,
      4150383,
      4097699,
      4058685,
      4316352,
      37162164,
      4009165,
      4282941,
      4342883,
      4059290,
      4174671,
      194990,
      4043254,
      42536741,
      42537674,
      4139925,
      4340950,
      4278462,
      4245954,
      4342778,
      37109995,
      4141669,
      45771255,
      4340391,
      4229773,
      4159144,
      4048520,
      37017093,
      4310146,
      4262316,
      4132088,
      4120949,
      4103295,
      4048521,
      4121463,
      4281749,
      321462,
      36713582,
      4187067,
      37163150,
      3655971,
      4180636,
      4311280,
      4024552,
      4166245,
      44784217,
      4239975,
      4263049,
      4134586,
      4164219,
      4027255,
      4178542,
      4134889
    ),
    addDescendantsToInclude = TRUE
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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
  saveName <- paste0("conditions_custom_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = condTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(condTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for procedure occurrences
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = procTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(procTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for visit occurrences (Overall visit & by visit concept count)
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = visitTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(visitTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for observations
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = obsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(obsTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for cohorts already created while running `01_buildCohorts`
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

  # Create covariate tibble for FE
  covariateCohorts <- as_tibble(analysisSettings[[1]]$cohorts$covariateCohorts)

  covariateCohorts <- covariateCohorts %>%
    dplyr::filter(id == covId) %>%
    dplyr::rename(
      cohortId = id,
      cohortName = name
    )

  # Create cohort settings
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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
      n = as.integer(n),
      pct = as.double(pct),
      analysisId = as.integer(analysisId),
      conceptId = as.integer(conceptId),
      covId = as.integer(covId)
                  ) %>%
    dplyr::select(-covariateId)

  # Output file name
  saveName <- paste0("cohort_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB), "_", covId)

  # Export
  verboseSave(
    object = cohortTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(cohortTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for cohorts already created while running `01_buildCohorts`
getCohortCustomFE <- function(con,
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

  # Create covariate tibble for FE
  covariateCohorts <- as_tibble(analysisSettings[[1]]$cohorts$covariateCohorts)

  covariateCohorts <- covariateCohorts %>%
    dplyr::filter(id == covId) %>%
    dplyr::rename(
      cohortId = id,
      cohortName = name
    )

  # Create cohort settings
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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
      n = as.integer(n),
      pct = as.double(pct),
      analysisId = as.integer(analysisId),
      conceptId = as.integer(conceptId),
      covId = as.integer(covId)
    ) %>%
    dplyr::select(-covariateId)

  # Output file name
  saveName <- paste0("cohort_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB), "_", covId)

  # Export
  verboseSave(
    object = cohortTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(cohortTbl)
}



# Function that runs function FeatureExtraction::createCovariateSettings for demographics (race, gender, ethnicity, index year, age group)
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

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  demoTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::collect() %>%
    dplyr::select(-covariateId) %>%
    dplyr::mutate(database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("demographics_baseline_", cohortId)

  # Export
  verboseSave(
    object = demoTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(demoTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for continuous variables (age, Charlson index, time in cohort)
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

  # If the cov$covariatesContinuous object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (purrr::is_null(cov$covariatesContinuous)) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

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

  # Export
  verboseSave(
    object = ctsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(ctsTbl)
}



## Main function  -----------------

executeConceptCharacterization <- function(con,
                                           type,
                                           runDrugs = FALSE,
                                           runConditions = FALSE,
                                           runVisits = FALSE,
                                           runDemographics = FALSE,
                                           runContinuous = FALSE,
                                           runProcedures = FALSE,
                                           runObservations = FALSE,
                                           runCohorts = FALSE,
                                           runCustom = FALSE,
                                           executionSettings,
                                           analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  # Get target cohort ids
  cohortKeyTarget <- analysisSettings[[1]]$cohorts$targetCohorts %>% dplyr::arrange(id)

  # Target ids (select cohorts that have enough counts i.e. 6 or more)
  strataCounts <- readr::read_csv(
      file = here::here("results", databaseId, "03_buildStrata", "strataCounts.csv"),
      show_col_types = FALSE
    )

  strataCounts2 <- strataCounts %>%
    dplyr::mutate(type =
                    dplyr::case_when(
                      is.na(subjects) ~ "noCounts",
                      subjects <= 5 ~ "lowCounts",
                      subjects > 5 ~ "enoughCounts"
                  )
    )

  cohortKey <- strataCounts2 %>%
    dplyr::filter(type == "enoughCounts") %>%
    dplyr::inner_join(cohortKeyTarget, by = c("id", "name")) %>%
    dplyr::select(id, name) %>%
    dplyr::arrange(id)

  # Get time windows
  timeA <- analysisSettings[[1]]$timeWindows$startDay
  timeB <- analysisSettings[[1]]$timeWindows$endDay

  # Output file and job names
  typeAnalysis <- as.data.frame(x = type) %>%
    dplyr::mutate(fullName =
                    dplyr::case_when(type == "postIndex" ~ "Post-Index",
                                     type == "baseline" ~ "Baseline",
                                     TRUE ~ type),
                  shortName =
                    dplyr::case_when(type == "postIndex" ~ "Post",
                                     type == "baseline" ~ "Base",
                                     TRUE ~ type)
                  )

  # Job Log
  cli::cat_boxx(crayon::magenta(paste0("Building ", typeAnalysis$fullName, " covariates")))
  cli::cat_bullet(paste0("Using ", typeAnalysis$fullName,  " window: [", crayon::green(timeA), ", ", crayon::green(timeB), "]"),
                  bullet = "info", bullet_col = "blue")
  cat_cohortId <- paste(cohortKey$id, collapse = ", ")
  cli::cat_bullet(paste0("Using cohort ids: ", crayon::green(cat_cohortId)), bullet = "info", bullet_col = "blue")
  cli::cat_line()

  tik <- Sys.time()


  if (type == "baseline") {

  ## Demographics ------
  if (runDemographics == TRUE) {

    # Calculate for each cohort
    purrr::pwalk(cohortKey,
                    ~ getDemographicsFE(con = con,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDatabaseSchema = workDatabaseSchema,
                                        cohortId = ..1,
                                        outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = "demoBase",
      pattern = "demographics_baseline"
    )

 }

  ## Continuous ------
  if (runContinuous == TRUE) {

    # Calculate for each cohort
    purrr::pwalk(cohortKey,
                    ~ getContinuousFE(con = con,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = workDatabaseSchema,
                                      cohortId = ..1,
                                      outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = "contBase",
      pattern = "continuous_baseline"
    )

   }
  }


  ## Drugs ------
  if (runDrugs == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("drugs", typeAnalysis$shortName),
      pattern = paste0("drugs_", typeAnalysis$type)
    )

  }

  ## Conditions ------
  if (runConditions == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cond", typeAnalysis$shortName),
      pattern = paste0("conditions_", typeAnalysis$type)
    )

  }

  ## Visits ------
  if (runVisits == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("visit", typeAnalysis$shortName),
      pattern = paste0("visits_", typeAnalysis$type)
    )

  }

  ## Procedures ------
  if (runProcedures == TRUE) {

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("proc", typeAnalysis$shortName),
      pattern = paste0("procedures_", typeAnalysis$type)
    )

  }

  ## Observations ------
  if (runObservations == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("obs", typeAnalysis$shortName),
      pattern = paste0("observations", typeAnalysis$type)
    )

  }


  ## Cohorts ------
  if (runCohorts == TRUE) {

    # Get covariate cohort ids
    covariateKey <- analysisSettings[[1]]$cohorts$covariateCohorts %>% dplyr::arrange(id)

    # Create grid df for execution
    cohortGrid <- createGrid2(
      cohortKey = cohortKey,
      covariateKey = covariateKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort, covariate and time window
    purrr::pwalk(cohortGrid,
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

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cohort", typeAnalysis$shortName),
      pattern = paste0("cohort_", typeAnalysis$type)
    )

  }

  ## Custom ------
  if (runCustom == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                 ~ getConditionsCustomFE(con = con,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         cohortTable = cohortTable,
                                         cohortDatabaseSchema = workDatabaseSchema,
                                         type = type,
                                         cohortId = ..1,
                                         timeA = ..3,
                                         timeB = ..4,
                                         outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("condCustom", typeAnalysis$shortName),
      pattern = paste0("conditions_custom_", typeAnalysis$type)
    )

  }

  # Job log
  tok <- Sys.time()
  cli::cat_bullet("Execution Completed at: ", crayon::red(tok), bullet = "info", bullet_col = "blue")
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  cli::cat_bullet("Execution took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  invisible(tok)
}

