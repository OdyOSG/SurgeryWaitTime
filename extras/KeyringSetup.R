# A. File Info ------------

# Task: Setup Credentials
# Keep your database credentials handy before running this script.
# Ask your database administrator if you are unsure what your credentials are.


# B. Dependencies ------------

library(tidyverse, quietly = TRUE)
library(keyring)
library(Ulysses)

# C. Set Parameters ------------

<<<<<<< HEAD
configBlock <- "configblock" # Name of config block

database <- "db" # Name of the database in the config block
=======
configBlock <- ""        # Name of config block

database <- ""           # Name of the database in the config block
>>>>>>> 2d3a3b9815edfc0d39597b98605e7ff954ee26f6

keyringPassword <- ""    # Password for keyring


# D. Check or create config.yml file ------------

## Check if config.yml file exists; create it if it doesn't;
Ulysses::checkConfig()

Ulysses::makeConfig(block = configBlock, database = database)


# E. Setup Keyring ------------

## Set keyring
setStudyKeyring(keyringName = basename(here::here()),
                keyringPassword = keyringPassword)

## Set credentials in keyring
setMultipleCredentials(cred = defaultCredentials(),
                       db = configBlock,
                       keyringName = basename(here::here()),
                       keyringPassword = keyringPassword,
                       forceCheck = TRUE
)

## If a single credential is incorrect, change it by running the function below:
# setCredential(cred = "dbms",
#                       db = configBlock,
#                       keyringName = basename(here::here()),
#                       keyringPassword = keyringPassword,
#                       forceCheck = TRUE
# )


# F. Check Credentials ------------

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = config::get("dbms", config = configBlock),
  user = config::get("user", config = configBlock),
  password = config::get("password", config = configBlock),
  connectionString = config::get("connectionString", config = configBlock)
)

# If this command returns the username set while running command 'setMultipleCredentials' above then the credentials have been stored in the keyring successfully
connectionDetails$user()
