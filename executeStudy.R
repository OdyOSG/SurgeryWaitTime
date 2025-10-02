# A. File Info -----------------------

# Study:
# Task: Execute Study


# B. Dependencies -----------------------

## Load libraries and scripts
source(here::here("analysis/private/_executeStudy.R"))


# C. Variables -----------------------

## Add database name
configBlock <- "[block]"

## Path to tasks folder
studyTaskFolder <- here::here("analysis/studyTasks")
studyTaskFiles <- fs::dir_ls(studyTaskFolder, type = "file")


# D. Scripts -----------------------

## Task 1: Build Cohorts
runStudyTask(file = studyTaskFiles[1], configBlock = configBlock)

## Task 2: Run Cohort Diagnostics
runStudyTask(file = studyTaskFiles[2], configBlock = configBlock)

## Task 3: Build Stratas
runStudyTask(file = studyTaskFiles[3], configBlock = configBlock)

## Task 4: Baseline Characteristics
runStudyTask(file = studyTaskFiles[4], configBlock = configBlock)

## Task 5: Post-index Characteristics
runStudyTask(file = studyTaskFiles[5], configBlock = configBlock)

## Task 6: Time-To-Event
runStudyTask(file = studyTaskFiles[6], configBlock = configBlock)

## Task 7: Zip Results
runStudyTask(file = studyTaskFiles[7], configBlock = configBlock)


