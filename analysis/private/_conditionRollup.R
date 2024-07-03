# A. File Info -----------------------

# Task: ICD10Chapter Rollup


# B. Sql -----------------

icd10AggSql <- "
    DROP TABLE IF EXISTS @targetDatabaseSchema.@icdCodesTable;

    CREATE TABLE @targetDatabaseSchema.@icdCodesTable AS
    WITH disease AS ( -- define disease categories similar to ICD10 Chapters
                SELECT 1 AS precedence, 'Blood disease' AS category_name, 440371 AS category_id
                UNION
                SELECT 1, 'Blood disease', 443723
                UNION
                SELECT 2, 'Injury and poisoning', 432795
                UNION
                SELECT 2, 'Injury and poisoning', 442562
                UNION
                SELECT 2, 'Injury and poisoning', 444363
                UNION
                SELECT 3, 'Congenital disease', 440508
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 435875
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4088927
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4154314
                UNION
                SELECT 4, 'Pregnancy or childbirth disease', 4136529
                UNION
                SELECT 5, 'Perinatal disease', 441406
                UNION
                SELECT 6, 'Infection', 432250
                UNION
                SELECT 7, 'Neoplasm', 438112
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 31821
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 4090739
                UNION
                SELECT 8, 'Endocrine or metabolic disease', 436670
                UNION
                SELECT 9, 'Mental disease', 432586
                UNION
                SELECT 10, 'Nerve disease and pain', 376337
                UNION
                SELECT 10, 'Nerve disease and pain', 4011630
                UNION
                SELECT 11, 'Eye disease', 4038502
                UNION
                SELECT 12, 'ENT disease', 4042836
                UNION
                SELECT 13, 'Cardiovascular disease', 134057
                UNION
                SELECT 14, 'Respiratory disease', 320136
                UNION
                SELECT 15, 'Digestive disease', 4302537
                UNION
                SELECT 16, 'Skin disease', 4028387
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4244662
                UNION
                SELECT 17, 'Soft tissue or bone disease', 433595
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4344497
                UNION
                SELECT 17, 'Soft tissue or bone disease', 40482430
                UNION
                SELECT 17, 'Soft tissue or bone disease', 4027384
                UNION
                SELECT 18, 'Genitourinary disease', 4041285
                UNION
                SELECT 19, 'Iatrogenic condition', 4105886
                UNION
                SELECT 19, 'Iatrogenic condition', 4053838
                )
    SELECT DISTINCT -- get the disease category with the lowest (best fitting) precedence, or assign 'Other Condition'
                    c.concept_id AS concept_id,
                    concept_name AS concept_name,
                    first_value(coalesce(category_id, 0))
                    OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_code,
                    first_value(coalesce(category_name, 'Other Condition'))
                    OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_name,
                    first_value(coalesce(precedence, 0))
                    OVER (PARTITION BY c.concept_id ORDER BY precedence, category_id NULLS LAST) AS category_id
    FROM @cdmDatabaseSchema.concept c
    JOIN (SELECT DISTINCT CAST(covariate_id / 1000 AS INT) AS concept_id FROM @targetDatabaseSchema.covariates) cs
        ON c.concept_id = cs.concept_id
    LEFT JOIN ( -- find the approprate disease category, if possible
              SELECT descendant_concept_id, category_id, category_name, precedence
              FROM @cdmDatabaseSchema.concept_ancestor
              JOIN disease
                  ON ancestor_concept_id = category_id
              ) D
        ON descendant_concept_id = c.concept_id;"


getIcd10Sql <- "
    SELECT cohort_id, category_id, category_code, category_name, count(*) AS countValue
    FROM (
      SELECT DISTINCT @cohortId AS cohort_id, row_id, category_id, category_code, category_name
      FROM @targetDatabaseSchema.covariates cs
      JOIN @targetDatabaseSchema.@icdCodesTable icd
          ON icd.concept_id = CAST(cs.covariate_id / 1000 AS INT)
          ) tab
    GROUP BY cohort_id, category_id, category_id, category_name, category_code
    ORDER BY category_id;"


# C. Functions ------------------------

## Helper functions -----------

getIcd10Chapters <- function(con,
                             cohortDatabaseSchema,
                             cohortTable,
                             cdmDatabaseSchema,
                             cohortId,
                             databaseId,
                             type,
                             timeA,
                             timeB) {

  # Job log
  cli::cat_bullet("Building ", crayon::green(type)," ICD-10 rollup, using time window [", crayon::green(timeA, ",", timeB), "]",
                 " for cohort id: ", crayon::green(cohortId),
                  bullet = "pointer", bullet_col = "yellow")
  cli::cat_line()


  # Define tables names
  targetCovariateTable <- "covariates"
  icdCodesTable <- "icd_codes"

  # Create condition group settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # If targetCovariate table persists for some reason
  cli::cat_bullet("1. Drop covariates table", bullet = "info", bullet_col = "blue")

  dropTableSql <- 'DROP TABLE IF EXISTS @targetDatabaseSchema.@targetCovariateTable'

  DatabaseConnector::renderTranslateExecuteSql(connection = con,
                                               sql = dropTableSql,
                                               targetDatabaseSchema = cohortDatabaseSchema,
                                               targetCovariateTable = targetCovariateTable)
  cli::cat_line()

  # Run FE and place results in covariates table
  cli::cat_bullet("2. Build temp covariates table in database", bullet = "info", bullet_col = "blue")

  FeatureExtraction::getDbDefaultCovariateData(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = paste(cohortDatabaseSchema, cohortTable, sep = "."),
    cohortIds = cohortId,
    covariateSettings = covSettings,
    targetDatabaseSchema = cohortDatabaseSchema,
    targetCovariateTable = targetCovariateTable,
    aggregated = FALSE
  )

  cli::cat_line()

  # Generate ICD Chapters features
  cli::cat_bullet("3. Rollup ICD Chapters features", bullet = "info", bullet_col = "blue")

  DatabaseConnector::renderTranslateExecuteSql(
    connection = con,
    sql = icd10AggSql,
    targetDatabaseSchema = cohortDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    icdCodesTable = icdCodesTable
  )

  cli::cat_line()

  # Retrieve ICD features
  cli::cat_bullet("4. Retrieve ICD Chapters features", bullet = "info", bullet_col = "blue")

  icdCovTab <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = getIcd10Sql,
    targetDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    icdCodesTable = icdCodesTable
  )

  cli::cat_line()

  # Format
  names(icdCovTab) <- tolower(names(icdCovTab))

  icdCovTab <- icdCovTab %>%
    dplyr::rename(
      cohortDefinitionId = cohort_id,
      analysisId = category_id,
      conceptId = category_code,
      name = category_name,
      n = countvalue
    ) %>%
    dplyr::mutate(timeWindow = paste0(abs(timeA), "_", abs(timeB)))


  # Remove temporary tables
  tabs <- c(targetCovariateTable, icdCodesTable)

  # Function to drop table
  dropTableSql <- function(t) {
    paste0('DROP TABLE IF EXISTS @targetDatabaseSchema.', t, ';\n')
  }

  # Build multiple SQL for table drop
  sql <- purrr::map_chr(tabs, ~dropTableSql(.x)) |>
    paste0(collapse = "")

  # Drop tables
  cli::cat_bullet("5. Clean up", bullet = "info", bullet_col = "blue")

  DatabaseConnector::renderTranslateExecuteSql(
    connection = con,
    sql = sql,
    targetDatabaseSchema = cohortDatabaseSchema
  )

  cli::cat_line()

  return(icdCovTab)
}

## Main function --------

executeConditionRollup <- function(con,
                                   type,
                                   executionSettings,
                                   analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  # Get target cohorts
  cohortKey <- analysisSettings[[1]]$cohorts$targetCohorts

  # Get time Windows
  timeA <- analysisSettings[[1]]$timeWindows$startDay
  timeB <- analysisSettings[[1]]$timeWindows$endDay

  # Create grid data frame for execution
  condGrid <- createGrid(cohortKey = cohortKey,
                         timeA = timeA,
                         timeB = timeB)

  # Output file and job names
  typeAnalysis <- as.data.frame(x = type) %>%
    dplyr::mutate(fullName =
                    dplyr::case_when(type == "postIndex" ~ "Post-Index",
                                     type == "baseline" ~ "Baseline",
                                     TRUE ~ NA),
                  shortName =
                    dplyr::case_when(type == "postIndex" ~ "Post",
                                     type == "baseline" ~ "Base",
                                     TRUE ~ NA)
    )

  # Check if the correct string for the 'type' argument has been inserted. Terminate function if not.
  if(is.na(typeAnalysis$fullName)) {

    cli::cat_bullet(crayon::red("Wrong type name inserted. Please add 'postIndex' or 'baseline' in the type argument."),
                    bullet = "info", bullet_col = "blue")

    return(NA)
  }

  # Job log
  cli::cat_boxx(crayon::magenta("Building ICD-10 Chapters rollup"))
  cli::cat_line()

  # Get ICD-10 chapters
  icd10ChapDat <- purrr::pmap_dfr(condGrid,
                                  ~getIcd10Chapters(con = con,
                                                    cohortDatabaseSchema = workDatabaseSchema,
                                                    cohortTable = cohortTable,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    cohortId = ..1,
                                                    databaseId = databaseId,
                                                    type = typeAnalysis$fullName,
                                                    timeA = ..3,
                                                    timeB = ..4)
  ) %>%
    dplyr::mutate(database = executionSettings$databaseName)


  # Export
  verboseSave(
    object = icd10ChapDat,
    saveName = paste0("condGroup", typeAnalysis$shortName),
    saveLocation = outputFolder
  )

  invisible(icd10ChapDat)
}
