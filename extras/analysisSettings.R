# A. File Info -----------------------

# Study:
# Task: Analysis Settings


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
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
      strataId = 1L:23L,
      strataName = c("0_9",
                     "10_19",
                     "20_29",
                     "30_39",
                     "40_49",
                     "50_59",
                     "60_69",
                     "70_79",
                     "80_89",
                     "90_99",
                     "100+",
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
yaml::write_yaml(yaml1, file = here::here("analysis/settings/strata.yml"), column.major = FALSE)


### All cohorts (target and its stratas) ------------------

demoStrata <- yaml1$strata$demographics

allCohorts <- expand_grid(targetCohorts, demoStrata) %>%
  dplyr::mutate(
    id = id * 1000 + strataId,
    name = paste(name, strataName)
  ) %>%
  dplyr::select(id, name) %>%
  rbind(targetCohorts) |>
  dplyr::mutate(
    id_first = as.integer(substr(as.character(id), 1, 1))
  )


## 2. Baseline Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(id == 9) |>                             # TO REMOVE
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml2 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,        # To replace with allCohorts
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
        startDay = c(-365L),
        endDay = c(-1L)
      ),
    'outputFolder' = fs::path("04_baselineCharacteristics/default")
  )
)

# Create yaml file
yaml::write_yaml(yaml2, file = here::here("analysis/settings/baseline.yml"), column.major = FALSE)


### Custom conditions
covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("NA")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml21 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,       # To replace with allCohorts
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(-9999L),
      endDay = c(-1L)
    ),
    'outputFolder' = fs::path("04_baselineCharacteristics/customConditions")
  )
)

# Create yaml file
yaml::write_yaml(yaml21, file = here::here("analysis/settings/baseline2.yml"), column.major = FALSE)


### BMI
covariateCohorts <- cohortManifest %>%
  dplyr::filter(name %in% c("BMI_gt_40", "BMI_less_15")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml22 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,       # To replace with allCohorts
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(-90L),
      endDay = c(-1L)
    ),
    'outputFolder' = fs::path("04_baselineCharacteristics/bmi")
  )
)

# Create yaml file
yaml::write_yaml(yaml22, file = here::here("analysis/settings/baseline3.yml"), column.major = FALSE)


### Hospitalization
covariateCohorts <- cohortManifest %>%
  dplyr::filter(name %in% c("hosp_wo_surgery")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml23 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,          # To replace with allCohorts
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(-183L),
      endDay = c(-1L)
    ),
    'outputFolder' = fs::path("04_baselineCharacteristics/hosp")
  )
)

# Create yaml file
yaml::write_yaml(yaml23, file = here::here("analysis/settings/baseline4.yml"), column.major = FALSE)


## 3. Post-Index Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml3 <- list(
  'postIndexCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,          # To replace with allCohorts
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
yaml::write_yaml(yaml3, file = here::here("analysis/settings/postIndex.yml"), column.major = FALSE)


## 4.1 Time To Event (Whole cohort) -------------------

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml4 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,    # To replace with allCohorts
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte")
  )
)

# Create yaml file
yaml::write_yaml(yaml4, file = here::here("analysis/settings/tte.yml"), column.major = FALSE)


## 4.2 Time To Event (Only Surgery patients) -------------------

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

baseCohorts <- allCohorts %>% dplyr::filter(id %in% c(4, 4012, 4013)) # TO REMOVE

yaml5 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = baseCohorts,      # To replace with allCohorts
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte2")
  )
)

# Create yaml file
yaml::write_yaml(yaml5, file = here::here("analysis/settings/tte2.yml"), column.major = FALSE)
