
createCohortsFromFiles <- function(connectionDetails,
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema,
                                   cohortTableName,
                                   cohortFileList,
                                   outputPath){
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  
  for(i in 1:length(cohortFileList)){
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