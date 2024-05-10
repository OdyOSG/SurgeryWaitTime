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

## All study Cohorts
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
      strataId = 1L:4L,
      strataName = c("below_65",
                     "65_and_above",
                     "male",
                     "female")
    ),
    'outputFolder' = fs::path("03_buildStrata")
  )
)

# Create yaml file
write_yaml(yaml1, file = here::here("analysis/settings/strata.yml"), column.major = FALSE)


#### All Cohorts ------------------

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

drugCohorts <- cohortManifest %>%
  dplyr::filter(type == c("era")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)


yaml3 <- list(
  'postIndexUtilization' = list(
    'cohorts' = list(
      'targetCohorts' = allCohorts,
      'covariateCohorts' = drugCohorts
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
