# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of PCVDosingSchedule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute OHDSI DOSETTE study
#'
#' @details
#' This function executes the OHDSI DOSETTE study.
#'
#' @param connectionDetails                    An object of type \code{connectionDetails} as created
#'                                             using the
#'                                             \code{\link[DatabaseConnector]{createConnectionDetails}}
#'                                             function in the DatabaseConnector package.
#' @param cdmDatabaseSchema                    Schema name where your patient-level data in OMOP CDM
#'                                             format resides. Note that for SQL Server, this should
#'                                             include both the database and schema name, for example
#'                                             'cdm_data.dbo'.
#' @param vocabularyDatabaseSchema             Schema name where your vocabulary tables in OMOP CDM format resides.
#'                                             Note that for SQL Server, this should include both the database and
#'                                             schema name, for example 'cdm_data.dbo'.
#' @param oracleTempSchema                     Should be used in Oracle to specify a schema where the
#'                                             user has write priviliges for storing temporary tables.
#' @param cohortDatabaseSchema                 Schema name where intermediate data can be stored. You
#'                                             will need to have write priviliges in this schema. Note
#'                                             that for SQL Server, this should include both the
#'                                             database and schema name, for example 'cdm_data.dbo'.
#' @param outputFolder                         Name of local folder to place results; make sure to use
#'                                             forward slashes (/). Do not use a folder on a network
#'                                             drive since this greatly impacts performance.
#' @param tablePrefix                          A prefix to be used for all table names created for this
#'                                             study.
#' @param databaseId                           A short string for identifying the database (e.g.
#'                                             'Synpuf').
#' @param databaseName                         The full name of the database (e.g. 'Medicare Claims
#'                                             Synthetic Public Use Files (SynPUFs)').
#' @param databaseDescription                  A short description (several sentences) of the database.
#' @param minCellCount                         The minimum cell count for fields contains person counts
#'                                             or fractions when exporting to CSV.
#' @param createExposureCohorts                Create the tables with the exposure cohorts?
#' @param createOutcomeCohorts                 Create the tables with the outcome cohorts?
#' @param createNegativeControlCohorts         Create the tables with the negative control outcome cohorts?
#' @param generateCensorAnalysisObjects        Manipulate cohort data into data frames for censor weight analysis?
#' @param pullAnalysisCohortsFromServer        Join exposure-outcome cohorts and download from server?
#' @param runCensorWeightAnalysis              Run the clone censor weight analysis for TTE? 
#' @param exportToCsv                          Export all results to CSV files?
#' @param maxCores                             How many parallel cores should be used? If more cores
#'                                             are made available this can speed up the analyses.
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    vocabularyDatabaseSchema = cdmDatabaseSchema,
                    oracleTempSchema = NULL,
                    cohortDatabaseSchema,
                    outputFolder,
                    tablePrefix = "dosette",
                    databaseId = "Unknown",
                    databaseName = "Unknown",
                    databaseDescription = "Unknown",
                    minCellCount = 5,
                    createExposureCohorts = TRUE,
                    createOutcomeCohorts = TRUE,
                    createNegativeControlCohorts = TRUE,
                    generateCensorAnalysisObjects = TRUE,
                    pullAnalysisCohortsFromServer = TRUE,
                    runCensorWeightAnalysis = TRUE,
                    exportToCsv = TRUE,
                    maxCores = 4) {
  
  if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
  }
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder, "log.txt"))
  ParallelLogger::addDefaultErrorReportLogger(file.path(outputFolder, "errorReportR.txt"))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_FILE_LOGGER", silent = TRUE))
  on.exit(ParallelLogger::unregisterLogger("DEFAULT_ERRORREPORT_LOGGER", silent = TRUE), add = TRUE)
  
  sinkFile <- file(file.path(outputFolder, "console.txt"), open = "wt")
  sink(sinkFile, split = TRUE)
  on.exit(sink(), add = TRUE)
  
  ParallelLogger::logInfo("Starting execute() for DOSETTE studies")
  
  if (createExposureCohorts) {
    exposureCohortFiles = c("inst/cohorts/LivebirthEligible.json",
                            "inst/cohorts/PCV13All.json")
    
    createCohortsFromFiles(connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTableName = paste(tablePrefix, "exposure_cohort", sep = "_"),
                           cohortFileList = exposureCohortFiles,
                           outputPath = outputFolder, 
                           indexPrefix = 0)
  }
  
  if (createOutcomeCohorts) {
    outcomeCohortFiles = list.files(path = "inst/cohorts/", pattern ="Hospi*", 
                                    full.names = TRUE) # "hospitalization outcome only for now"
    createCohortsFromFiles(connectionDetails = connectionDetails,
                           cdmDatabaseSchema = cdmDatabaseSchema,
                           cohortDatabaseSchema = cohortDatabaseSchema,
                           cohortTableName = paste(tablePrefix, "outcome_cohort", sep = "_"),
                           cohortFileList = outcomeCohortFiles,
                           outputPath = outputFolder, 
                           indexPrefix = 10) # outcome cohort IDs will be 11, 12, 13, 14
  }
  
  if (createNegativeControlCohorts){
    negativeControlCsvPath = "inst/settings/NegativeControls.csv"
    createNegativeControlsCohorts(connectionDetails = connectionDetails,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortDatabaseSchema = cohortDatabaseSchema,
                                  cohortTableName = paste(tablePrefix, "outcome_cohort", sep = "_"),
                                  negativeControlsCsv = negativeControlCsvPath,
                                  outputPath = outputFolder)
  }
  
  if (generateCensorAnalysisObjects) {
    ## join the birth cohort and the PCV cohort
    deriveBirthVaccinationCohort(connectionDetails = connectionDetails,
                                 cohortDatabaseSchema = cohortDatabaseSchema,
                                 cohortTableName = paste(tablePrefix, "exposure_cohort", sep = "_"))
    
    ## adding covariates to big cohort table
    queryCohortCovariates(connectionDetails = connectionDetails, 
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          resultCohortTableName = paste(tablePrefix, "cohort_covariates", sep = "_"),
                          outputPath = outputFolder)
  }
  
  if (pullAnalysisCohortsFromServer) {
    ## join the exposure-outcome cohort tables and download to local csv files
    pullAllJointCohorts(connectionDetails = connectionDetails,
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        exposureCohortTableName = paste(tablePrefix, "cohort_covariates", sep = "_"),
                        outcomeCohortTableName = paste(tablePrefix, "outcome_cohort", sep = "_"), 
                        outcomeCohortListFile = paste(tablePrefix, "outcome_cohort_cohortCounts.csv", sep = "_"),
                        outputPath = outputFolder,
                        pullFromServer = TRUE)
  }
  
  if (runCensorWeightAnalysis) {
    cohortFolder = file.path(outputFolder, "cohorts")
    tteOutputFolder = file.path(outputFolder, "tteResults")
    
    runTTEBatch(cohortPath = cohortFolder, 
                outputPath = NULL, 
                nsims = 200, 
                seed = 351)
    
  }

  
  if (exportToCsv) {
    tteOutputFolder = file.path(outputFolder, "tteResults")
    zip::zip(zipfile = file.path(tteOutputFolder, "allResults.zip"), 
             files = list.files(tteOutputFolder, full.names = TRUE))
    
    ParallelLogger::logInfo(paste("All TTE results are compressed at", file.path(tteOutputFolder, "allResults.zip")))
  }

  
  ParallelLogger::logInfo("Finished execute() for DOSETTE studies")
}

