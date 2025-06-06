# A. File Info -----------------------

# Study:
# Task: Zip Results


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
source(here::here("analysis/private/_utilities.R"))


# C. Connection ----------------------

## Set connection block
# <<<
configBlock <- "[block]"
# >>>


# E. Script --------------------

zipResults(database = executionSettings$databaseName)
