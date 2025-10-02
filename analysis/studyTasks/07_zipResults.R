# A. File Info -----------------------

# Study: Surgery Wait Time
# Task: Zip Results


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = T)
source(here::here("analysis/private/_utilities.R"))


# C. Connection ----------------------

## Set connection Block
# <<<
configBlock <- "[block]"
# >>>


# D. Script --------------------

zipResults()
