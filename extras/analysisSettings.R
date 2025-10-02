# A. File Info -----------------------

# Study:
# Task: Analysis Settings


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = T)
library(yaml)
source(here::here("analysis/private/_utilities.R"))


# C. Script --------------------

## All study cohorts
cohortManifest <- getCohortManifest()

## Target Cohorts
targetCohorts <- cohortManifest %>%
  dplyr::filter(type == c("target")) %>%
  dplyr::filter(stringr::str_detect(name, "no Prior Cancer Treatment")) |>
  dplyr::filter(!stringr::str_detect(name, "after")) |>
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
yaml::write_yaml(yaml1, file = here::here("analysis/settings/strata.yml"), column.major = F)


### All cohorts (target and its strata) ------------------

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

allCohortsChar <- cohortManifest %>%
  dplyr::filter(type == c("target")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id) |>
  rbind(allCohorts[,1:2]) |>
  dplyr::distinct()


## 2. Baseline Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml2 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
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
yaml::write_yaml(yaml2, file = here::here("analysis/settings/baseline.yml"), column.major = F)


### Custom conditions
covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("NA")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml21 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
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
yaml::write_yaml(yaml21, file = here::here("analysis/settings/baseline2.yml"), column.major = F)


### BMI
covariateCohorts <- cohortManifest %>%
  dplyr::filter(name %in% c("BMI_gt_40", "BMI_less_15")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml22 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
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
yaml::write_yaml(yaml22, file = here::here("analysis/settings/baseline3.yml"), column.major = F)


### Hospitalization
covariateCohorts <- cohortManifest %>%
  dplyr::filter(name %in% c("hosp_wo_surgery")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml23 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
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
yaml::write_yaml(yaml23, file = here::here("analysis/settings/baseline4.yml"), column.major = F)


## 3. Post-Index Characteristics --------------------

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml3 <- list(
  'postIndexCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
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
yaml::write_yaml(yaml3, file = here::here("analysis/settings/postIndex.yml"), column.major = F)


## 4.1 Time To Event (Single Curve) -------------------

### Colorectal ---------------

targetCohorts <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Colorectal") & stringr::str_detect(name,"within"))

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "colorectal")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml411 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/singleCurve")
  )
)

# Create yaml file
yaml::write_yaml(yaml411, file = here::here("analysis/settings/tte_col_single.yml"), column.major = F)


### Esophagus ---------------

targetCohorts <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Esophagus") & stringr::str_detect(name,"within"))

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "esophagus")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml412 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/singleCurve")
  )
)

# Create yaml file
yaml::write_yaml(yaml412, file = here::here("analysis/settings/tte_eso_single.yml"), column.major = F)


### Lung ---------------

targetCohorts <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Lung") & stringr::str_detect(name,"within"))

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "lung")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml413 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/singleCurve")
  )
)

# Create yaml file
yaml::write_yaml(yaml413, file = here::here("analysis/settings/tte_lung_single.yml"), column.major = F)


### Breast ---------------

targetCohorts <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Breast") & stringr::str_detect(name,"within"))

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "breast")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml414 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/singleCurve")
  )
)

# Create yaml file
yaml::write_yaml(yaml414, file = here::here("analysis/settings/tte_breast_single.yml"), column.major = F)


## 4.2 Time To Event (Multiple Curves) ----------

### Colorectal ----------

targetCohorts_sex <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Colorectal") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(12, 13)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Sex")

targetCohorts_race <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Colorectal") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(14, 15, 16, 17)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Race")

targetCohorts_ethnicity <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Colorectal") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(18, 19, 20)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Ethnicity")

targetCohorts_cci <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Colorectal") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(21, 22, 23)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "CCI")

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "colorectal")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml421 <- list(
  'tte_sex' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_sex,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Sex",
    'cancerType' = "Colorectal"
  ),
  'tte_race' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_race,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Race",
    'cancerType' = "Colorectal"
  ),
  'tte_ethnicity' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_ethnicity,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Ethnicity",
    'cancerType' = "Colorectal"
  ),
  'tte_cci' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_cci,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "CCI",
    'cancerType' = "Colorectal"
  )
)

# Create yaml file
yaml::write_yaml(yaml421, file = here::here("analysis/settings/tte_col_multiple.yml"), column.major = F)


### Lung ----------

targetCohorts_sex <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Lung") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(12, 13)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Sex")

targetCohorts_race <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Lung") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(14, 15, 16, 17)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Race")

targetCohorts_ethnicity <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Lung") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(18, 19, 20)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Ethnicity")

targetCohorts_cci <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Lung") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(21, 22, 23)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "CCI")

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "lung")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml422 <- list(
  'tte_sex' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_sex,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Sex",
    'cancerType' = "Lung"
  ),
  'tte_race' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_race,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Race",
    'cancerType' = "Lung"
  ),
  'tte_ethnicity' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_ethnicity,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Ethnicity",
    'cancerType' = "Lung"
  ),
  'tte_cci' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_cci,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "CCI",
    'cancerType' = "Lung"
  )
)

# Create yaml file
yaml::write_yaml(yaml422, file = here::here("analysis/settings/tte_lung_multiple.yml"), column.major = F)


### Breast ----------

targetCohorts_sex <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Breast") & stringr::str_detect(name,"within")) |>
  #dplyr::filter(stringr::str_detect(as.character(id), "12|13")) |>
  dplyr::filter(id %% 100 %in% c(12, 13)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Sex")

targetCohorts_race <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Breast") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(14, 15, 16, 17)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Race")

targetCohorts_ethnicity <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Breast") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(18, 19, 20)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Ethnicity")

targetCohorts_cci <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Breast") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(21, 22, 23)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "CCI")

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "breast")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml423 <- list(
  'tte_sex' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_sex,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Sex",
    'cancerType' = "Breast"
  ),
  'tte_race' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_race,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Race",
    'cancerType' = "Breast"
  ),
  'tte_ethnicity' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_ethnicity,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Ethnicity",
    'cancerType' = "Breast"
  ),
  'tte_cci' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_cci,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "CCI",
    'cancerType' = "Breast"
  )
)

# Create yaml file
yaml::write_yaml(yaml423, file = here::here("analysis/settings/tte_breast_multiple.yml"), column.major = F)


### Esophagus ----------

targetCohorts_sex <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Esophagus") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(12, 13)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Sex")

targetCohorts_race <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Esophagus") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(14, 15, 16, 17)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Race")

targetCohorts_ethnicity <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Esophagus") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(18, 19, 20)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "Ethnicity")

targetCohorts_cci <- allCohorts |>
  dplyr::filter(stringr::str_detect(name, "Esophagus") & stringr::str_detect(name,"within")) |>
  dplyr::filter(id %% 100 %in% c(21, 22, 23)) |>
  dplyr::filter(!id %in% c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22)) |>
  dplyr::mutate(category = "CCI")

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::filter(stringr::str_detect(name, "esophagus")) |>
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml424 <- list(
  'tte_sex' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_sex,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Sex",
    'cancerType' = "Esophagus"
  ),
  'tte_race' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_race,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Race",
    'cancerType' = "Esophagus"
  ),
  'tte_ethnicity' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_ethnicity,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "Ethnicity",
    'cancerType' = "Esophagus"
  ),
  'tte_cci' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts_cci,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte/multipleCurves"),
    'category'     = "CCI",
    'cancerType' = "Esophagus"
  )
)

# Create yaml file
yaml::write_yaml(yaml424, file = here::here("analysis/settings/tte_eso_multiple.yml"), column.major = F)


## 4.3 Time To Event (Only Surgery patients) -------------------

eventCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcomeSurgeries")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml43 <- list(
  'tte' = list(
    'cohorts' = list(
      'targetCohorts' = allCohortsChar,
      'eventCohorts' = eventCohorts
    ),
    'outputFolder' = fs::path("06_tte2")
  )
)

# Create yaml file
yaml::write_yaml(yaml43, file = here::here("analysis/settings/tte2.yml"), column.major = F)


