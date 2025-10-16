# load the study package
library(PCVDosingSchedule)

#### ---- Customize code below depending on your data server and local directories ---- ####
# set up driver path
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="s:/DatabaseDrivers") # specify your path here

# Run-once: set-up your database driver
DatabaseConnector::downloadJdbcDrivers(dbms = "postgresql")
# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "s:/AndromedaTemp")

# Maximum number of cores to be used:
maxCores <- min(4, parallel::detectCores()) # Or more depending on your hardware

# Patch for Oracle (if necessary)
oracleTempSchema <- NULL

# The folder where the study intermediate and result files will be written:
outputFolder <- "s:/DosetteStudyOutput" # Specify your output folder here!

# Details for connecting to the server:
# See ?DatabaseConnector::createConnectionDetails for help
conn <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                   server = "some.server.com/ohdsi",
                                                   user = "joe",
                                                   password = "secret")

# The name of the database schema where the CDM data can be found:
cdmDatabaseSchema <- "cdm_synpuf"
# The name of the database schema and table where the study-specific cohorts will be instantiated:
cohortDatabaseSchema <- "scratch.dbo"
tablePrefix <- "dosette_study"

# Some meta-information that will be used by the export function:
databaseId <- "Synpuf"
databaseName <- "Medicare Claims Synthetic Public Use Files (SynPUFs)"
databaseDescription <- "Medicare Claims Synthetic Public Use Files (SynPUFs) were created to allow interested parties to gain familiarity using Medicare claims data while protecting beneficiary privacy. These files are intended to promote development of software and applications that utilize files in this format, train researchers on the use and complexities of Centers for Medicare and Medicaid Services (CMS) claims, and support safe data mining innovations. The SynPUFs were created by combining randomized information from multiple unique beneficiaries and changing variable values. This randomization and combining of beneficiary information ensures privacy of health information."

# For some database platforms (e.g. Oracle): define a schema that can be used to emulate temp tables:
options(sqlRenderTempEmulationSchema = NULL)


#### ---- Run the study package ---- ####
execute(connectionDetails = conn,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createExposureCohorts = TRUE,
        createOutcomeCohorts = TRUE,
        createNegativeControlCohorts = TRUE,
        generateCensorAnalysisObjects = TRUE,
        pullAnalysisCohortsFromServer = TRUE,
        runCensorWeightAnalysis = FALSE,
        exportToCsv = FALSE,
        maxCores = maxCores)
