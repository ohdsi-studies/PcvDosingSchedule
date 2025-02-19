
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
  
  for(i in c(1:length(cohortFileList)) + indexPrefix){
    cohortJsonFileName <- cohortFileList[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression,
                                          options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- rbind(cohortsToCreate,
                             data.frame(cohortId = i,
                                        cohortName = cohortName,
                                        sql = cohortSql,
                                        stringsAsFactors = FALSE))
    
  }
  
  
  # create cohort table
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableName)
  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                      cohortTableNames = cohortTableNames)
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(connectionDetails = connectionDetails,
                                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                                         cohortDatabaseSchema = cohortDatabaseSchema,
                                                         cohortTableNames = cohortTableNames,
                                                         cohortDefinitionSet = cohortsToCreate)
  # Get cohort counts
  cohortCounts <- CohortGenerator::getCohortCounts(connectionDetails = connectionDetails,
                                                   cohortDatabaseSchema = cohortDatabaseSchema,
                                                   cohortTable = cohortTableNames$cohortTable)
  countsFileName = paste0(cohortTableName, "_cohortCounts.csv")
  readr::write_csv(cohortCounts, file.path(outputPath, countsFileName))
}

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