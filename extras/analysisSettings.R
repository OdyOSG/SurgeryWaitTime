# A. File Info -----------------------

# Study:
# Task: Analysis Settings


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(yaml)
source(here::here("analysis/private/_utilities.R"))


# C. Script --------------------

## All study cohorts
cohortManifest <- getCohortManifest()

## Target Cohorts
targetCohorts <- cohortManifest %>%
  dplyr::filter(type == c("target")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)


## 1. Stratas --------------------

yaml1 <- list(
  'strata' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'strataCohorts' = targetCohorts
    ),
    'demographics' = tibble::tibble(
      strataId = 1L:16L,
      strataName = c("below_65",
                     "65_and_above",
                     "male",
                     "female",
                     "beforeMar20",
                     "afterMar20",
                     "black",
                     "white",
                     "asian",
                     "unknown_race",
                     "hispanic",
                     "notHispanic",
                     "unknown_ethnicity",
                     "cci_0",
                     "cci_1",
                     "cci_2")
    ),
    'outputFolder' = fs::path("03_buildStrata")
  )
)

# Create yaml file
write_yaml(yaml1, file = here::here("analysis/settings/strata.yml"), column.major = FALSE)


#### All cohorts (target and its stratas) ------------------

demoStrata <- yaml1$strata$demographics

allCohorts <- expand_grid(targetCohorts, demoStrata) %>%
  dplyr::mutate(
    id = id * 1000 + strataId,
    name = paste(name, strataName)
  ) %>%
  dplyr::select(id, name) %>%
  rbind(targetCohorts)


## 2. Baseline Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type == c("covariate")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#allCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1001, 1002, 1003))

yaml2 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
        startDay = c(-365L, -365L),
        endDay = c(-1L, 0)
      ),
    'outputFolder' = fs::path("04_baselineCharacteristics")
  )
)

# Create yaml file
write_yaml(yaml2, file = here::here("analysis/settings/baseline.yml"), column.major = FALSE)


## 3. Post-Index Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("covariate", "outcome")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#allCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1001, 1002, 1003))

yaml3 <- list(
  'postIndexUtilization' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(0, 0),
      endDay = c(99999, 183)
      ),
    'outputFolder' = fs::path("05_postIndexUtilization")
  )
)

# Create yaml file
write_yaml(yaml3, file = here::here("analysis/settings/postIndex.yml"), column.major = FALSE)


## 4. Treatment Pathways Analysis-------------------

txCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcome", "covariate")) %>%
  #dplyr::filter(type %in% c("outcome")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml4 <- list(
  'treatmentPatterns' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'txCohorts' = txCohorts
    ),
    'treatmentHistorySettings' = list(
      minEraDuration = 0L,
      eraCollapseSize = 30L,
      combinationWindow = 30L,
      minPostCombinationDuration = 30L,
      filterTreatments = "First",
      periodPriorToIndex = 0L,
      includeTreatments = "startDate",
      maxPathLength = 5L,
      minCellCount = 5L,
      minCellMethod = "Remove",
      groupCombinations = 10L,
      addNoPaths = FALSE
    ),
    'outputFolder' = list(
      fs::path("06_treatmentHistory"),
      fs::path("07_tte")
    )
  )
)

# Create yaml file
write_yaml(yaml4, file = here::here("analysis/settings/tte.yml"), column.major = FALSE)
