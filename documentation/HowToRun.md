# How to run the SurgeryWaitTime study

## Setup Study

### Download Zip

Download and run the package by following the instructions below:

1)  Go to the [Github repo webpage](https://github.com/OdyOSG/SurgeryWaitTime/tree/main)
2)  Click on the green `Code` button revealing a dropdown menu
3)  Click on `Download Zip`
4)  Unzip the file on your computer
5)  Open the unzipped folder and open file `SurgeryWaitTime.Rproj` with RStudio

### Setup R Environment

#### Using `renv`

The study uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) to install all the necessary packages for execution. To activate `renv` run the following command:

``` r
renv::restore()
```

**NOTE**: To test if the `renv` package installation ran successfully run `renv::status()`. If it has, you should see the following message printed in the console:\
\
`No issues found -- the project is in a consistent state.`

#### Instructions to add GitHub Personal Access Token in R

Several OHDSI packages required for the package execution can only be downloaded from GitHub. In order to download those packages with `renv` you'll need to authenticate your Github account via a Personal Access Token (PAT). Follow the instructions below to create a Personal Access Token and to add to R:

1.  Log in to [Github](https://github.com/) with your account

2.  Go to page <https://github.com/settings/tokens>

3.  Click on `Generate new token` (from the dropdown select classic)

4.  Give a name to your token and set an expiration date (It is recommended to set the maximum date allowed i.e. 365 days)

5.  From the `Select Scopes` list select only `repo`

6.  Scroll down to the bottom of the page and click on `Generate Token`

7.  **CAUTION:** Make sure you keep the next page open until you have copied and pasted the token (a string e.g. *a1b2c3d4e5f6g7h8g9h0ijklmnopqrstuvwxyz*. You won't be able to see it again after closing the page.

8.  Go to `RStudio` and run command `file.edit("~/.Renviron")`. This will open the `.Renviron` file in the editor (Note that if the file doesn't exist, it will be created).

9.  Create a variable named `GITHUB_PAT` and assign the generate token in step 7. Save and restart R.

**NOTE:** Adding the PAT in Github and R has to be done only once. You won't have to generate a new token for future study packages that are using `renv` and require installation of OHDSI packages from Github.

#### Troubleshooting `renv`

There might be errors with the package installation via `renv`. If you encounter an error with a package try removing it from the `renv.lock` file and restore again. To remove a package from the lock file find the header of the package and delete all corresponding lines. Once you get the remaining packages to install, manually install the package(s) using one of the options below:

``` r

# Installing an R package from CRAN ------------

## Installing latest version of R package on CRAN
install.packages("ggplot2")

## Installing archived version of R package on CRAN
packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.9.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

# Installing an R package from Github -----------------

# Installing current version of R package from github
install.packages("remotes") 
remotes::install_github("ohdsi/FeatureExtraction")

# Installing develop version of R package from github
remotes::install_github("ohdsi/Ulysses", ref = "develop")

# Installing old version of R package from github
remotes::install_github("ohdsi/CohortGenerator", ref = "v0.7.0")
```

### Load Execution Credentials

The package uses `keyring` and `config` to mask and query the credentials needed for execution.

#### Required Credentials

In order to execute the package the following credentials are required:

1)  **dbms** - The name of the dbms you are using (redshift, postgresql, snowflake, etc.)
2)  **user** - The username credential used to connect to the OMOP database
3)  **password** - The password credential used to connect to the OMOP database
4)  **connectionString** - A composed string that establishes the connection to the OMOP database. An example of a connection string would be *jdbc:dbms://host-url.com:port/database_name*.
5)  **cdmDatabaseSchema** - The database and schema where the OMOP CDM data are located. Note that this credential may be separated by a dot to indicate the database and schema, which is the case in SQL Server. For example: *your_database.your_schema*.
6)  **vocabDatabaseSchema** - The database and schema where the OMOP CDM data vocabulary tables are located. Note that this is typically the same as `cdmDatabaseSchema`.
7)  **workDatabaseSchema** - The database and schema where the user has read/write permission rights

It is recommended that you store these credentials in a text file to make it easier to load into the credential manager.

#### Loading Credentials

1)  Open file `extras/KeyringSetup.R`
2)  On L16:18 place a name for your config block and database. The `configBlock` variable name can be an abbreviation for the database. For example:

``` r
configBlock <- "synpuf"
database    <- "synpuf_110k"
```

3)  One at a time run each line in the script and follow any prompts

#### Troubleshooting

If you encounter problems with `keyring`, you can avoid it by hard-coding your credentials to the `config.yml` file as shown below:

``` yml
db:                # Replace with an abbreviation for your database (no underscores or spaces)
databaseName: <db> # Replace with the database name in the server
dbms: <your_dbms_dialect>
user:  <your_username>
password:  <your_password>
connectionString: <your_connectionString>
cdmDatabaseSchema:  <your_cdmDatabaseSchema>
vocabDatabaseSchema:  <your_vocabDatabaseSchema>
workDatabaseSchema: <your_workDatabaseSchema>
cohortTable: swt_<databaseName> 
```

## Run Study

### Execution Script

Running all the analytical tasks can be done using the `executeStudy.R` file. Replace L16 with the `configBlock` (database) of choice and run the rest of the script.

### Study Tasks

The study contains two tasks:

1)  **Build Cohorts** - Creates cohorts from the JSON files in the `cohortsToCreate` folder
2)  **Cohort Diagnostics** - Generates Cohort Diagnostics data

Each task will output files in a folder named `results`. Note that the first task (`buildCohorts`) is required to run any additional task.

## Review Cohort Diagnostics results

Once the package execution is complete, open file `extras/ExploreDiagnostics.R` and run it, following the instructions, in order to launch the Cohort Diagnostics shiny app.
