# A. File Info -----------------------

# Task: Build Stratas


# B. Functions ------------------------

ageStrata <- function(con,
                      cohortDatabaseSchema,
                      cohortTable,
                      cdmDatabaseSchema,
                      targetId,
                      strataId,
                      ageMin,
                      ageMax) {


  cli::cat_bullet(crayon::green("Building age strata between ", ageMin, "-" ,ageMax))

  sql <- "
          SELECT
          t2.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
          t2.subject_id,
          t2.cohort_start_date,
          t2.cohort_end_date
        INTO #age
        FROM (
          SELECT
            t1.cohort_definition_id,
            t1.subject_id,
            t1.cohort_start_date,
            t1.cohort_end_date,
            CASE
              WHEN age between @ageMin and @ageMax THEN 1
              ELSE 0
            END AS ageStrata
          FROM (
            SELECT c.cohort_definition_id,
                   c.subject_id,
                   c.cohort_start_date,
                   c.cohort_end_date,
                   p.year_of_birth,
                   abs(p.year_of_birth - YEAR(c.cohort_start_date)) AS age
            FROM @cohortDatabaseSchema.@cohortTable c
            JOIN @cdmDatabaseSchema.person p
              ON p.person_id = c.subject_id
            WHERE c.cohort_definition_id IN (@targetId)
            ) t1
          ) t2
        WHERE t2.ageStrata = 1;

        DELETE FROM @cohortDatabaseSchema.@cohortTable
        WHERE cohort_definition_id in (select cohort_definition_id from #age);

        INSERT INTO @cohortDatabaseSchema.@cohortTable (
              	cohort_definition_id,
              	subject_id,
              	cohort_start_date,
              	cohort_end_date
        )
        select * from #age;

        DROP TABLE #age;"

  ageStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    targetId = targetId,
    strataId = strataId,
    ageMin = ageMin,
    ageMax = ageMax) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, ageStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet("Age strata written to ", cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(ageStrataSql)
}



genderStrata <- function(con,
                         cohortDatabaseSchema,
                         cohortTable,
                         cdmDatabaseSchema,
                         targetId,
                         strataId,
                         gender = c("male", "female")) {

  if (gender == "male")
    {

      cli::cat_bullet(crayon::green("Building Male strata"))

      sql <- "
              SELECT
                t1.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
                t1.subject_id,
                t1.cohort_start_date,
                t1.cohort_end_date
              INTO #male
              FROM (
                  SELECT c.cohort_definition_id,
                         c.subject_id,
                         c.cohort_start_date,
                         c.cohort_end_date,
                         p.gender_concept_id,
                         CASE
                            WHEN p.gender_concept_id = 8507 THEN 'M'
                            WHEN p.gender_concept_id = 8532 THEN 'F'
                            ELSE 'N'
                        END AS gender
                  FROM @cohortDatabaseSchema.@cohortTable c
                  JOIN @cdmDatabaseSchema.person p
                    ON p.person_id = c.subject_id
                  WHERE c.cohort_definition_id IN (@targetId)
              ) t1
              WHERE t1.gender = 'M';

              DELETE FROM @cohortDatabaseSchema.@cohortTable
              WHERE cohort_definition_id in (select cohort_definition_id from #male);

              INSERT INTO @cohortDatabaseSchema.@cohortTable (
                    	cohort_definition_id,
                    	subject_id,
                    	cohort_start_date,
                    	cohort_end_date
              )
              select * from #male;

              DROP TABLE #male; "

  } else if (gender == "female") {

    cli::cat_bullet(crayon::green("Building Female strata"))

    sql <- "
            SELECT
              t1.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
              t1.subject_id,
              t1.cohort_start_date,
              t1.cohort_end_date
            INTO #female
            FROM (
                SELECT c.cohort_definition_id,
                       c.subject_id,
                       c.cohort_start_date,
                       c.cohort_end_date,
                       p.gender_concept_id,
                       CASE
                          WHEN p.gender_concept_id = 8507 THEN 'M'
                          WHEN p.gender_concept_id = 8532 THEN 'F'
                          ELSE 'N'
                      END AS gender
                FROM @cohortDatabaseSchema.@cohortTable c
                JOIN @cdmDatabaseSchema.person p
                  ON p.person_id = c.subject_id
                WHERE c.cohort_definition_id IN (@targetId)
            ) t1
            WHERE t1.gender = 'F';

            DELETE FROM @cohortDatabaseSchema.@cohortTable
            WHERE cohort_definition_id in (select cohort_definition_id from #female);

            INSERT INTO @cohortDatabaseSchema.@cohortTable (
                  	cohort_definition_id,
                  	subject_id,
                  	cohort_start_date,
                  	cohort_end_date
            )
            select * from #female;

            DROP TABLE #female; "

  }

  genderStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    targetId = targetId,
    strataId = strataId) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, genderStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet(paste0(tools::toTitleCase(gender), " strata written to "), cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(genderStrataSql)
}



dateStrata <- function(con,
                       cohortDatabaseSchema,
                       cdmDatabaseSchema,
                       cohortTable,
                       targetId,
                       strataId,
                       dateMin,
                       dateMax) {

  cli::cat_bullet(crayon::green("Building date strata between", dateMin, "and" , dateMax, "(Date Format: MM-DD-YYYY)"))

  sql <- "
          SELECT
          t2.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
          t2.subject_id,
          t2.cohort_start_date,
          t2.cohort_end_date
        INTO #date
        FROM (
          SELECT
            c.cohort_definition_id,
            c.subject_id,
            c.cohort_start_date,
            c.cohort_end_date
          FROM @cohortDatabaseSchema.@cohortTable c
          WHERE
            c.cohort_definition_id IN (@targetId) AND
            c.cohort_start_date between '@dateMin' AND '@dateMax'
        ) t2;

        DELETE FROM @cohortDatabaseSchema.@cohortTable
        WHERE cohort_definition_id in (select cohort_definition_id from #date);

        INSERT INTO @cohortDatabaseSchema.@cohortTable (
              	cohort_definition_id,
              	subject_id,
              	cohort_start_date,
              	cohort_end_date
        )
        select * from #date;

        DROP TABLE #date;"

  dateStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    targetId = targetId,
    strataId = strataId,
    dateMin = dateMin,
    dateMax = dateMax) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, dateStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet("Date strata written to ", cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(dateStrataSql)
}


raceStrata <- function(con,
                       cohortDatabaseSchema,
                       cohortTable,
                       cdmDatabaseSchema,
                       targetId,
                       strataId,
                       conceptId) {

    cli::cat_bullet(crayon::green("Building Race strata: ", as.character(conceptId)))

    sql <- "
              SELECT
                t1.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
                t1.subject_id,
                t1.cohort_start_date,
                t1.cohort_end_date
              INTO #race
              FROM (
                  SELECT c.cohort_definition_id,
                         c.subject_id,
                         c.cohort_start_date,
                         c.cohort_end_date,
                         p.race_concept_id
                  FROM @cohortDatabaseSchema.@cohortTable c
                  JOIN @cdmDatabaseSchema.person p
                    ON p.person_id = c.subject_id
                  WHERE c.cohort_definition_id IN (@targetId)
              ) t1
              WHERE t1.race_concept_id in (@raceConceptId);


              DELETE FROM @cohortDatabaseSchema.@cohortTable
              WHERE cohort_definition_id in (select cohort_definition_id from #race);

              INSERT INTO @cohortDatabaseSchema.@cohortTable (
                    	cohort_definition_id,
                    	subject_id,
                    	cohort_start_date,
                    	cohort_end_date
              )
              select * from #race;

              DROP TABLE #race; "

  raceStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    raceConceptId = conceptId,
    targetId = targetId,
    strataId = strataId) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, raceStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet("Race strata written to ", cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(raceStrataSql)
}


measurementStrata <- function(con,
                             cohortDatabaseSchema,
                             cohortTable,
                             cdmDatabaseSchema,
                             targetId,
                             strataId,
                             conceptId,
                             value) {

  cli::cat_bullet(crayon::green("Building Measurement strata: ", as.character(conceptId)))

  sql <- "
              SELECT
                t1.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
                t1.subject_id,
                t1.cohort_start_date,
                t1.cohort_end_date
              INTO #measurement
              FROM (
                  SELECT c.cohort_definition_id,
                         c.subject_id,
                         c.cohort_start_date,
                         c.cohort_end_date,
                         p.measurement_concept_id,
                         p.value_as_number
                  FROM @cohortDatabaseSchema.@cohortTable c
                  JOIN @cdmDatabaseSchema.measurement p
                    ON p.person_id = c.subject_id
                  WHERE c.cohort_definition_id IN (@targetId)
              ) t1
              WHERE t1.measurement_concept_id in (@measurementConceptId) and t1.value_as_number in (@value);


              DELETE FROM @cohortDatabaseSchema.@cohortTable
              WHERE cohort_definition_id in (select cohort_definition_id from #measurement);

              INSERT INTO @cohortDatabaseSchema.@cohortTable (
                    	cohort_definition_id,
                    	subject_id,
                    	cohort_start_date,
                    	cohort_end_date
              )
              select * from #measurement;

              DROP TABLE #measurement;"

  measurementStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    measurementConceptId = conceptId,
    value = value,
    targetId = targetId,
    strataId = strataId) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, measurementStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet("Measurement strata written to ", cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(measurementStrataSql)
}


ethnicityStrata <- function(con,
                       cohortDatabaseSchema,
                       cohortTable,
                       cdmDatabaseSchema,
                       targetId,
                       strataId,
                       conceptId) {

  cli::cat_bullet(crayon::green("Building Ethnicity strata: ", as.character(conceptId)))

  sql <- "
              SELECT
                t1.cohort_definition_id * 1000 + @strataId AS cohort_definition_id,
                t1.subject_id,
                t1.cohort_start_date,
                t1.cohort_end_date
              INTO #ethnicity
              FROM (
                  SELECT c.cohort_definition_id,
                         c.subject_id,
                         c.cohort_start_date,
                         c.cohort_end_date,
                         p.ethnicity_concept_id
                  FROM @cohortDatabaseSchema.@cohortTable c
                  JOIN @cdmDatabaseSchema.person p
                    ON p.person_id = c.subject_id
                  WHERE c.cohort_definition_id IN (@targetId)
              ) t1
              WHERE t1.ethnicity_concept_id in (@ethnicityConceptId);


              DELETE FROM @cohortDatabaseSchema.@cohortTable
              WHERE cohort_definition_id in (select cohort_definition_id from #ethnicity);

              INSERT INTO @cohortDatabaseSchema.@cohortTable (
                    	cohort_definition_id,
                    	subject_id,
                    	cohort_start_date,
                    	cohort_end_date
              )
              select * from #ethnicity;

              DROP TABLE #ethnicity;"

  ethnicityStrataSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    cdmDatabaseSchema = cdmDatabaseSchema,
    ethnicityConceptId = conceptId,
    targetId = targetId,
    strataId = strataId) %>%
    SqlRender::translate(targetDialect = con@dbms)

  DatabaseConnector::executeSql(connection = con, ethnicityStrataSql, progressBar = FALSE)

  cohortStrataId <- targetId * 1000 + strataId
  cohortSchemaTable <- paste(cohortDatabaseSchema, cohortTable, sep = ".")

  cli::cat_bullet("Ethnicity strata written to ", cohortSchemaTable, " using ids: ", crayon::red(paste(cohortStrataId, collapse = ", ")),
                  bullet = "tick", bullet_col = "green")
  cat(" ","\n")

  invisible(ethnicityStrataSql)
}


buildStrata <- function(con,
                        executionSettings,
                        analysisSettings) {

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings$strata$outputFolder) %>%
    fs::dir_create()

  # Get cohort and strata ids
  targetCohorts <- analysisSettings$strata$cohorts$targetCohorts
  demoStratas <- analysisSettings$strata$demographics


  ## Age ---------------
  cli::cat_rule("Building Age Strata")

  ## Age strata: Below 65
  ageStrata(con,
            cohortDatabaseSchema = workDatabaseSchema,
            cohortTable = cohortTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            targetId = targetCohorts$id,
            strataId = demoStratas$strataId[1],
            ageMin = 0,
            ageMax = 64)

  ## Age strata: 65 and above
  ageStrata(con,
            cohortDatabaseSchema = workDatabaseSchema,
            cohortTable = cohortTable,
            cdmDatabaseSchema = cdmDatabaseSchema,
            targetId = targetCohorts$id,
            strataId = demoStratas$strataId[2],
            ageMin = 65,
            ageMax = 150)


  ## Gender ---------------
  cli::cat_rule("Building Gender Strata")

  ## Gender strata: Male
  genderStrata(con,
               cohortDatabaseSchema = workDatabaseSchema,
               cohortTable = cohortTable,
               cdmDatabaseSchema = cdmDatabaseSchema,
               targetId = targetCohorts$id,
               strataId = demoStratas$strataId[3],
               gender = c("male"))

  ## Gender strata: Female
  genderStrata(con,
               cohortDatabaseSchema = workDatabaseSchema,
               cohortTable = cohortTable,
               cdmDatabaseSchema = cdmDatabaseSchema,
               targetId = targetCohorts$id,
               strataId = demoStratas$strataId[4],
               gender = c("female"))


  ## Date ---------------
  cli::cat_rule("Building Date Strata")

  ## Date strata: Before March 2020 (Format for Snowflake: MM-DD-YYYY)
  dateStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cdmDatabaseSchema = cdmDatabaseSchema,
             cohortTable = cohortTable,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[5],
             dateMin = "01-01-1989",
             dateMax = "03-01-2020")

  ## Date strata: After March 2020 (Format for Snowflake: MM-DD-YYYY)
  dateStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cdmDatabaseSchema = cdmDatabaseSchema,
             cohortTable = cohortTable,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[6],
             dateMin = "03-02-2020",
             dateMax = "01-01-2024")

  ## Race ---------------
  cli::cat_rule("Building Race Strata")

  ## Race strata: Black
  raceStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cohortTable = cohortTable,
             cdmDatabaseSchema = cdmDatabaseSchema,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[7],
             conceptId = 8516)

  ## Race strata: White
  raceStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cohortTable = cohortTable,
             cdmDatabaseSchema = cdmDatabaseSchema,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[8],
             conceptId = 8527)

  ## Race strata: Asian
  raceStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cohortTable = cohortTable,
             cdmDatabaseSchema = cdmDatabaseSchema,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[9],
             conceptId = 8515)

  ## Race strata: Unknown
  raceStrata(con,
             cohortDatabaseSchema = workDatabaseSchema,
             cohortTable = cohortTable,
             cdmDatabaseSchema = cdmDatabaseSchema,
             targetId = targetCohorts$id,
             strataId = demoStratas$strataId[10],
             conceptId = 0)

  ## Ethnicity ---------------
  cli::cat_rule("Building Ethnicity Strata")

  ## Ethnicity strata: Hispanic or Latino
  ethnicityStrata(con,
                 cohortDatabaseSchema = workDatabaseSchema,
                 cohortTable = cohortTable,
                 cdmDatabaseSchema = cdmDatabaseSchema,
                 targetId = targetCohorts$id,
                 strataId = demoStratas$strataId[11],
                 conceptId = 38003563)

  ## Ethnicity strata: Not Hispanic or Latino
  ethnicityStrata(con,
                  cohortDatabaseSchema = workDatabaseSchema,
                  cohortTable = cohortTable,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  targetId = targetCohorts$id,
                  strataId = demoStratas$strataId[12],
                  conceptId = 38003564)

  ## Ethnicity strata: Unknown
  ethnicityStrata(con,
                  cohortDatabaseSchema = workDatabaseSchema,
                  cohortTable = cohortTable,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  targetId = targetCohorts$id,
                  strataId = demoStratas$strataId[13],
                  conceptId = 0)

  ## Measurement ---------------
  cli::cat_rule("Building Measurement Strata")

  ## Measurement strata: CHA2DS2-VASc score
  measurementStrata(con,
                    cohortDatabaseSchema = workDatabaseSchema,
                    cohortTable = cohortTable,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    targetId = targetCohorts$id,
                    strataId = demoStratas$strataId[14],
                    conceptId = 37017409,
                    value = 0)

  measurementStrata(con,
                    cohortDatabaseSchema = workDatabaseSchema,
                    cohortTable = cohortTable,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    targetId = targetCohorts$id,
                    strataId = demoStratas$strataId[15],
                    conceptId = 37017409,
                    value = 1)

  measurementStrata(con,
                    cohortDatabaseSchema = workDatabaseSchema,
                    cohortTable = cohortTable,
                    cdmDatabaseSchema = cdmDatabaseSchema,
                    targetId = targetCohorts$id,
                    strataId = demoStratas$strataId[16],
                    conceptId = 37017409,
                    value = c(2:6))


  # Strata cohort names and ids
  tb1 <- expand_grid(targetCohorts, demoStratas) %>%
    dplyr::mutate(
      cohortId = id * 1000 + strataId,
      cohortName = paste(name, strataName)
    ) %>%
    dplyr::select(cohortId, cohortName)

  # Target cohort names and ids
  tb2 <- targetCohorts %>%
    dplyr::rename(
      cohortName = name,
      cohortId = id
    )

  # Bind strata and cohort ids and names
  cohortNamesIds <- rbind(tb1, tb2)

  # Get cohort counts
  sql <- "
          SELECT
            cohort_definition_id as id,
            count(distinct subject_id) as subjects,
            count(subject_id) as entries
          FROM @cohortDatabaseSchema.@cohortTable
          WHERE cohort_definition_id IN (@cohortIds)
          GROUP BY cohort_definition_id;"

  renderedSql <- SqlRender::render(
    sql,
    cohortDatabaseSchema = workDatabaseSchema,
    cohortTable = cohortTable,
    cohortIds = cohortNamesIds$cohortId) %>%
    SqlRender::translate(targetDialect = con@dbms)

  cohortCounts <- DatabaseConnector::querySql(connection = con, sql = renderedSql, snakeCaseToCamelCase = TRUE)

  # Join counts and names/ids
  dt <- cohortNamesIds %>%
    dplyr::left_join(cohortCounts, by = c("cohortId" = "id")) %>%
    dplyr::rename(
      id = cohortId,
      name = cohortName
    ) %>%
    dplyr::mutate(database = executionSettings$databaseName)

  # Export
  verboseSave(
    object = dt,
    saveName = "strataCounts",
    saveLocation = outputFolder
  )

  invisible(dt)
}

