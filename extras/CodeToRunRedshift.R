# rJava::.jinit(parameters="-Xmx100g", force.init = TRUE)
# options(java.parameters = c("-Xms200g", "-Xmx200g"))

library(PCVDosingSchedule)

Sys.setenv(DATABASECONNECTOR_JAR_FOLDER="E:/Drivers")
options(andromedaTempFolder = "E:/andromedaTemp")
oracleTempSchema <- NULL

#### DATABASE CONNECTION DETAILS -------

# ## CCAE
# cdmDatabaseSchema <- "cdm_truven_ccae_v3046"
# serverSuffix <- "truven_ccae"
# cohortDatabaseSchema <- "scratch_fbu2"
# databaseId <- "CCAE"
# databaseName <- "IBM Health MarketScan Commercial Claims and Encounters Database"
# databaseDescription <- "IBM Health MarketScan® Commercial Claims and Encounters Database (CCAE) represent data from individuals enrolled in United States employer-sponsored insurance health plans. The data includes adjudicated health insurance claims (e.g. inpatient, outpatient, and outpatient pharmacy) as well as enrollment data from large employers and health plans who provide private healthcare coverage to employees, their spouses, and dependents. Additionally, it captures laboratory tests for a subset of the covered lives. This administrative claims database includes a variety of fee-for-service, preferred provider organizations, and capitated health plans."
# tablePrefix <- "legend_t2dm_ccae"
# outputFolder <- "E:/FBu/Dosette_ccae_output"

# ## OptumEHR
# cdmDatabaseSchema <- "cdm_optum_ehr_v3037"
# serverSuffix <- "optum_ehr"
# cohortDatabaseSchema <- "scratch_fbu2"
# databaseId <- "OptumEHR"
# databaseName <- "Optum© de-identified Electronic Health Record Dataset"
# databaseDescription <- "Optum© de-identified Electronic Health Record Dataset represents Humedica’s Electronic Health Record data a medical records database. The medical record data includes clinical information, inclusive of prescriptions as prescribed and administered, lab results, vital signs, body measurements, diagnoses, procedures, and information derived from clinical Notes using Natural Language Processing (NLP)."
# tablePrefix <- "legend_t2dm_optum_ehr"
# outputFolder <- "E:/FBu/Dosette_optum_ehr_output"

# ## MDCD (Medicaid)
# cdmDatabaseSchema <- "cdm_truven_mdcd_v3038"
# serverSuffix <- "truven_mdcd"
# cohortDatabaseSchema <- "scratch_fbu2"
# databaseId<- "MDCD"
# databaseName <- "IBM Health MarketScan® Multi-State Medicaid Database"
# databaseDescription <- "IBM MarketScan® Multi-State Medicaid Database (MDCD) adjudicated US health insurance claims for Medicaid enrollees from multiple states and includes hospital discharge diagnoses, outpatient diagnoses and procedures, and outpatient pharmacy claims as well as ethnicity and Medicare eligibility. Members maintain their same identifier even if they leave the system for a brief period however the dataset lacks lab data."
# tablePrefix <- "legend_t2dm_mdcd"
# outputFolder <-  "E:/FBu/Dosette_mdcd_output"

## Optum DoD
cdmDatabaseSchema <- "cdm_optum_extended_dod_v3039"
serverSuffix <- "optum_extended_dod"
cohortDatabaseSchema <- "scratch_fbu2"
databaseId <- "OptumDod"
databaseName <- "Optum Clinformatics Extended Data Mart - Date of Death (DOD)"
databaseDescription <- "Optum Clinformatics Extended DataMart is an adjudicated US administrative health claims database for members of private health insurance, who are fully insured in commercial plans or in administrative services only (ASOs), Legacy Medicare Choice Lives (prior to January 2006), and Medicare Advantage (Medicare Advantage Prescription Drug coverage starting January 2006).  The population is primarily representative of commercial claims patients (0-65 years old) with some Medicare (65+ years old) however ages are capped at 90 years.  It includes data captured from administrative claims processed from inpatient and outpatient medical services and prescriptions as dispensed, as well as results for outpatient lab tests processed by large national lab vendors who participate in data exchange with Optum.  This dataset also provides date of death (month and year only) for members with both medical and pharmacy coverage from the Social Security Death Master File (however after 2011 reporting frequency changed due to changes in reporting requirements) and location information for patients is at the US state level."
tablePrefix <- "legend_t2dm_optum_dod"
outputFolder <-  "E:/FBu/Dosette_optum_dod_output"


#### Connection credentials -----
conn <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  server = paste0(keyring::key_get("epi_server"), "/", !!serverSuffix),
  port = 5439,
  user = Sys.getenv("REDSHIFT_USER"), #keyring::key_get("redshiftUser"),
  password = Sys.getenv("REDSHIFT_PASSWORD"), #keyring::key_get("redshiftPassword"),
  extraSettings = "ssl=true&sslfactory=com.amazon.redshift.ssl.NonValidatingFactory",
  pathToDriver = 'E:/Drivers')


## trying connecting to database server (DO NOT RUN!!) 
# connection = DatabaseConnector::connect(conn)
# DatabaseConnector::disconnect(connection)


## the `execute` main function ----
## (pulling exposure-outcome cohort table function only, if all cohorts are created)
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

## (the whole pipeline up to pulling all data to local; no analyses run yet)
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

