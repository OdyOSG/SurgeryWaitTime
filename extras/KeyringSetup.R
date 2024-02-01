# A. File Info ------------

# Task: Setup Credentials
# Keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure what your credentials are.


# B. Dependencies ------------

library(tidyverse, quietly = TRUE)
library(Ulysses)
library(keyring)


# C. Set Parameters ------------

configBlock <- "synpuf" # Name of config block

database <- "synpuf" # Name of the database in the config block

keyringPassword <- "odyosg"       # Password for keyring


# D. Check or create Config File ------------

## Check if config.yml file exists; create it if it doesn't; 
Ulysses::checkConfig()

Ulysses::makeConfig(block = configBlock, database = database)


# E. Setup Keyring ------------

## Set keyring
setStudyKeyring(keyringName = basename(here::here()),
                keyringPassword = keyringPassword)

## Set credential keys in keyring
setMultipleCredentials(cred = defaultCredentials(),
                       db = configBlock,
                       keyringName = basename(here::here()),
                       keyringPassword = keyringPassword,
                       forceCheck = TRUE
)

## If a single credential is incorrect, change it
# setCredential(cred = "dbms",
#                       db = configBlock,
#                       keyringName = basename(here::here()),
#                       keyringPassword = keyringPassword,
#                       forceCheck = TRUE
# )


# F. Check Credentials ------------

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  user = "ohdsi",
  password = "ohdsi",
  server = "testnode.arachnenetwork.com/synpuf_110k",
  port = 5441
)

connectionDetails$dbms

