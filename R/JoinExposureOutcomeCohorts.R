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
# 
#' @export
joinExposureOutcomeCohorts <- function(connectionDetails,
                                       cohortDatabaseSchema,
                                       exposureCohortTableName,
                                       outcomeCohortTableName,
                                       outcomeId,
                                       outputPath,
                                       pullFromServer = TRUE,
                                       oracleTempSchema = NULL){
  
  
  if(!dir.exists(file.path(outputPath, "cohorts"))){
    dir.create(file.path(outputPath, "cohorts"))
  }
  
  localFile = file.path(outputPath, "cohorts", 
                        sprintf("cohorts_outcome_%s.csv", outcomeId))
  
  ## pass if local file already saved...
  if(file.exists(localFile)){
    ParallelLogger::logInfo(sprintf("Local cohort table already generated for outcomeId = %s", outcomeId))
    return()
  }
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  ParallelLogger::logInfo(sprintf("Join exposure and outcome cohorts for outcomeId = %s", outcomeId))
  sql <- SqlRender::loadRenderTranslateSql("JoinExposureOutcomeCohorts.sql",
                                           "PCVDosingSchedule",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           exposure_cohort_table = exposureCohortTableName,
                                           outcome_cohort_table = outcomeCohortTableName,
                                           outcome_id = outcomeId)
  DatabaseConnector::executeSql(connection, sql)
  
  ## get counts 
  sql <- sprintf("SELECT COUNT(*) FROM %s.dosette_exposure_outcome_cohort;",
                 cohortDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  theCount = DatabaseConnector::querySql(connection, sql)$COUNT
  ParallelLogger::logInfo(sprintf("Found %s subjects in final cohort.", theCount))
  
  
  ## pull from server and save as CSV file
  if(pullFromServer){
    ParallelLogger::logInfo("Downloading cohort table from server.")
    
    sql <- sprintf("SELECT * FROM %s.dosette_exposure_outcome_cohort;",
                   cohortDatabaseSchema)
    sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
    dat = DatabaseConnector::querySql(connection, sql)
    #names(dat) = SqlRender::snakeCaseToCamelCase(names(dat))
    names(dat) = tolower(names(dat))
    readr::write_csv(dat, localFile)
    
    ParallelLogger::logInfo(sprintf("Data saved at %s", localFile))
  }

}



#' @export
pullAllJointCohorts <- function(connectionDetails,
                                cohortDatabaseSchema,
                                exposureCohortTableName,
                                outcomeCohortTableName,
                                outputPath,
                                outcomeCohortListFile,
                                pullFromServer = TRUE,
                                oracleTempSchema = NULL){
  
  if(!dir.exists(file.path(outputPath, "cohorts"))){
    dir.create(file.path(outputPath, "cohorts"))
  }
  
  outcomeCohortList = read.csv(file.path(outputPath,outcomeCohortListFile))$cohortId
  
  ParallelLogger::logInfo("Pulling all exposure-outcome joint cohort tables from server.")
  
  for (id in outcomeCohortList){
    joinExposureOutcomeCohorts(connectionDetails = connectionDetails,
                               cohortDatabaseSchema = cohortDatabaseSchema,
                               exposureCohortTableName = exposureCohortTableName,
                               outcomeCohortTableName = outcomeCohortTableName,
                               outcomeId = id,
                               outputPath = outputPath,
                               pullFromServer = pullFromServer,
                               oracleTempSchema = oracleTempSchema)
  }
  
  ParallelLogger::logInfo(sprintf("Saved all joint cohort tables at %s", file.path(outputPath, "cohorts")))
  
}