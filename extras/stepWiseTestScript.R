
#### DO NOT RUN ------
## Test script for package development

library(PCVDosingSchedule)

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="E:/Drivers")
options(andromedaTempFolder = "E:/andromedaTemp")
oracleTempSchema <- NULL


## OptumEHR
cdmDatabaseSchema <- "cdm_optum_ehr_v3037"
serverSuffix <- "optum_ehr"
cohortDatabaseSchema <- "scratch_fbu2"
databaseId <- "OptumEHR"
databaseName <- "Optum© de-identified Electronic Health Record Dataset"
databaseDescription <- "Optum© de-identified Electronic Health Record Dataset represents Humedica’s Electronic Health Record data a medical records database. The medical record data includes clinical information, inclusive of prescriptions as prescribed and administered, lab results, vital signs, body measurements, diagnoses, procedures, and information derived from clinical Notes using Natural Language Processing (NLP)."
tablePrefix <- "legend_t2dm_optum_ehr"
outputFolder <- "E:/FBu/Dosette_optum_ehr_output"

#### Connection credentials -----
conn <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("epi_server"), "/", !!serverSuffix),
  port = 5439,
  user = Sys.getenv("REDSHIFT_USER"), #keyring::key_get("redshiftUser"),
  password = Sys.getenv("REDSHIFT_PASSWORD"), #keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
  pathToDriver = 'E:/Drivers')


#### test cohort creation functions
exposureCohortFiles = c("inst/cohorts/LivebirthEligible.json",
                        "inst/cohorts/PCV13All.json")
outcomeCohortFiles = list.files(path = "inst/cohorts/", pattern ="Hospi*", full.names = TRUE)

createCohortsFromFiles(connectionDetails = conn, 
                       cdmDatabaseSchema = cdmDatabaseSchema, 
                       cohortDatabaseSchema = cohortDatabaseSchema,
                       cohortTableName = "dosette_outcome_cohort", 
                       cohortFileList = outcomeCohortFiles,
                       outputPath = outputFolder, indexPrefix = 10)


negativeControlCsvPath = "inst/settings/NegativeControls.csv"
createNegativeControlsCohorts(connectionDetails = conn,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              cohortTableName = "dosette_outcome_cohort",
                              negativeControlsCsv = negativeControlCsvPath,
                              outputPath = outputFolder)

## try deriving the birth-vaccine cohort
deriveBirthVaccinationCohort(connectionDetails = conn, 
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTableName = "dosette_exposure_cohorts")

## try adding covariates to big cohort table
queryCohortCovariates(connectionDetails = conn, 
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      resultCohortTableName = "dosette_cohort_covariates",
                      outputPath = outputFolder)

## try pulling one joined exposure-outcome cohort table
joinExposureOutcomeCohorts(connectionDetails = conn,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           exposureCohortTableName = "dosette_cohort_covariates",
                           outcomeCohortTableName = "dosette_outcome_cohort",
                           outcomeId = 11, # main outcomes: 11-14
                           pullFromServer = TRUE,
                           outputPath = outputFolder)

## test the TTE analysis function by Kayoko's group 

testOutcomeFile = "cohorts_outcome_11.csv"
cohortPath = file.path(outputFolder, "cohorts")

out <- data_tte_process(inputFilePath = file.path(cohortPath, testOutcomeFile), 
                        outputPath = file.path(outputFolder, "tteResults"),
                        
                        # Simulation parameters
                        nsims = 1000,
                        seed = 42,
                        
                        # Define the interval for the timing of PCV Dose 1
                        recDose1Start = 38, # in days 
                        recDose1End = 92, # in days
                        
                        # Define the interval for the timing of PCV Dose 2
                        recDose2Start = 113,
                        recDose2End = 141,
                        
                        # Define the interval for the timing of PCV Dose 3
                        recDose3Start = 173,
                        recDose3End = 201,
                        
                        # Define the interval for the timing of PCV Dose 4 (Booster dose)
                        recDose4Start = 358,
                        recDose4End = 476,
                        
                        #----------Reduced 1+1 Protocol----------#
                        
                        # Define the interval for the timing of PCV Dose 1
                        reduDose1Start = 38,
                        reduDose1End = 92,
                        
                        # Define the interval for the timing of PCV Dose 2 (Booster dose)
                        reduDose2Start = 358,
                        reduDose2End = 476,
                        
                        #---------- Modeling ----------#
                        
                        # Establish acceptable threshold for missing data in models 
                        missingnessThreshold = 0.70)

## test the `execute` main function ----
## (first testing the pulling exposure-outcome cohort table function)
execute(connectionDetails = conn,
        cdmDatabaseSchema = cdmDatabaseSchema,
        cohortDatabaseSchema = cohortDatabaseSchema,
        outputFolder = outputFolder,
        databaseId = databaseId,
        databaseName = databaseName,
        databaseDescription = databaseDescription,
        createExposureCohorts = FALSE,
        createOutcomeCohorts = FALSE,
        createNegativeControlCohorts = FALSE,
        generateCensorAnalysisObjects = FALSE,
        pullAnalysisCohortsFromServer = TRUE,
        runCensorWeightAnalysis = FALSE,
        exportToCsv = FALSE,
        maxCores = 4)

## (test the whole pipeline up to pulling all data to local)
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
        maxCores = 4)