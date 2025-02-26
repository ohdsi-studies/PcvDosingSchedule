
#' @export 
createCohortsFromFiles <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTableName,
                                   cohortFileList,
                                   outputPath,
                                   indexPrefix = 100){
  
  mes = paste("Creating cohorts in table", cohortTableName)
  ParallelLogger::logInfo(mes)
  
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  
  for(i in c(1:length(cohortFileList))){
    cohortJsonFileName <- cohortFileList[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression,
                                          options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- rbind(cohortsToCreate,
                             data.frame(cohortId = i+indexPrefix,
                                        cohortName = cohortName,
                                        sql = cohortSql,
                                        stringsAsFactors = FALSE))
    
  }
  
  
  # create cohort table
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableName)
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTableNames = cohortTableNames)
  
  ParallelLogger::logInfo("cohort table created.")
  
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTableNames = cohortTableNames,
                                                         cohortDefinitionSet = cohortsToCreate)
  # Get cohort counts
  ParallelLogger::logInfo("getting cohort counts")
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTableNames$cohortTable)
  countsFileName = paste0(cohortTableName, "_cohortCounts.csv")
  readr::write_csv(cohortCounts, file.path(outputPath, countsFileName))
}

#' @export 
createNegativeControlsCohorts <- function(connectionDetails,
                                          cdmDatabaseSchema,
                                          cohortDatabaseSchema,
                                          cohortTableName,
                                          negativeControlsCsv,
                                          outputPath,
                                          oracleTempSchema = NULL){
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  ParallelLogger::logInfo("Creating negative control outcome cohorts")
  negativeControls <- read.csv(negativeControlsCsv)
  data <- negativeControls[, c("conceptId", "cohortId")]
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "#negative_controls",
                                 data = data,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema)
  sql <- SqlRender::loadRenderTranslateSql("NegativeControls.sql",
                                           "PCVDosingSchedule",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           target_database_schema = cohortDatabaseSchema,
                                           target_cohort_table = cohortTableName)
  DatabaseConnector::executeSql(connection, sql)
  
  sql <- "TRUNCATE TABLE #negative_controls; DROP TABLE #negative_controls;"
  sql <- SqlRender::translate(sql = sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  DatabaseConnector::executeSql(connection, sql, progressBar = FALSE, 
                                reportOverallTime = FALSE)
  
  # get counts 
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTableName)
  countsFileName = paste0(cohortTableName, "_cohortCounts.csv")
  readr::write_csv(cohortCounts, file.path(outputPath, countsFileName))
}



#' @export
deriveBirthVaccinationCohort <- function(connectionDetails,
                                         cohortDatabaseSchema,
                                         cohortTableName,
                                         oracleTempSchema = NULL){
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  ParallelLogger::logInfo("Generating derived cohort table with birth date and vaccination dates")
  sql <- SqlRender::loadRenderTranslateSql("DeriveBirthVaccinationCohort.sql",
                                           "PCVDosingSchedule",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           cohort_table = cohortTableName)
  DatabaseConnector::executeSql(connection, sql)
  
  # get counts 
  sql <- sprintf("SELECT COUNT(*) FROM %s.dosette_birth_vaccine_cohort;",cohortDatabaseSchema)
  # sql <- SqlRender::renderSql(sql, 
  #                             cohort_database_schema = cohortDatabaseSchema)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  theCount = DatabaseConnector::querySql(connection, sql)$COUNT
  ParallelLogger::logInfo(sprintf("Found %s subjects in derived cohort.", theCount))
}



#' @export
queryCohortCovariates <- function(connectionDetails,
                                  cdmDatabaseSchema,
                                  cohortDatabaseSchema,
                                  resultCohortTableName,
                                  outputPath,
                                  oracleTempSchema = NULL){
  
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  ParallelLogger::logInfo("Querying demographic and geographic covariates of all subjects.")
  sql <- SqlRender::loadRenderTranslateSql("QueryCohortCovariates.sql",
                                           "PCVDosingSchedule",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           cohort_database_schema = cohortDatabaseSchema,
                                           result_cohort_table = resultCohortTableName)
  DatabaseConnector::executeSql(connection, sql)
  
  ## count missing data proportions for all fields
  ParallelLogger::logInfo("Counting missing data proportions.")
  sql <- SqlRender::loadRenderTranslateSql("CountMissingProportions.sql",
                                           "PCVDosingSchedule",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cohort_database_schema = cohortDatabaseSchema)
  missingProps = DatabaseConnector::querySql(connection, sql)
  names(missingProps) = SqlRender::snakeCaseToCamelCase(names(missingProps))
  countsFileName = paste0(resultCohortTableName, "_missingCounts.csv")
  readr::write_csv(missingProps, file.path(outputPath, countsFileName))
}
