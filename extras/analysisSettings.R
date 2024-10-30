# A. File Info -----------------------

# Study:
# Task: Analysis Settings


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
library(yaml)
source("analysis/private/_utilities.R")


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
      strataId = 1L:14L,
      strataName = c("below_65",
                     "65_and_above",
                     "male",
                     "female",
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


### All cohorts (target and its stratas) ------------------

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
  dplyr::filter(type %in% c("outcomeSurgeries", "outcomeDrugs")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1002, 1003))

yaml2 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
        startDay = c(-365L),
        endDay = c(-1L)
      ),
    'outputFolder' = fs::path("04_baselineCharacteristics")
  )
)

# Create yaml file
write_yaml(yaml2, file = here::here("analysis/settings/baseline.yml"), column.major = FALSE)


## 3. Post-Index Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries", "outcomeDrugs")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#postCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1002, 1003))

yaml3 <- list(
  'postIndexCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(1, 0, 1, 0),
      endDay = c(9999, 9999, 183, 183)
      ),
    'outputFolder' = fs::path("05_postIndexCharacteristics")
  )
)

# Create yaml file
write_yaml(yaml3, file = here::here("analysis/settings/postIndex.yml"), column.major = FALSE)


## 4.1 Time To Event (Whole cohort) -------------------

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries", "outcomeDrugs")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#targetCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1002, 1003))

yaml4 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = list(
      fs::path("06_tte")
    )
  )
)

# Create yaml file
write_yaml(yaml4, file = here::here("analysis/settings/tte.yml"), column.major = FALSE)


## 4.2 Time To Event (Only Surgery patients) -------------------

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

#targetCohorts <- allCohorts %>% dplyr::filter(id %in% c(1, 1002, 1003))

yaml5 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = list(
      fs::path("07_tte2")
    )
  )
)

# Create yaml file
write_yaml(yaml5, file = here::here("analysis/settings/tte2.yml"), column.major = FALSE)
