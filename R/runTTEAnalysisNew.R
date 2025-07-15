################################################################################
#                                                                              #
#   PROJECT: DOSETTE PCV - Evaluation of PCV dosing schedules using OHDSI data # 
#                                                                              #
#      DATE: Feb 2025 (version June 19, 2025)                                  #
#  CODED BY: Shae Gantt (RA) and Kayoko Shioda (PI)                            #          
#    E-MAIL: kshioda@bu.edu                                                    #  
#                                                                              #
################################################################################

#------------------------------------------------------------------------------#
# Description 

# Function equivalent of DOSETTE PCV
# Programmer for modified code: Nina Cesare, Shae Gantt
# Program modified: 04/15/2025
# Additional modification: 04/16/2025 - Output now only contains plot data and plot object.
# Additional modification: 04/17/2025 - Modified plot data objects to summarize across simulations and generate 95% CIs
# Additional modification: 04/18/2025 - Reorganized plot data object order. Created function to select formula based on the creation of state
# Additional modification: 05/16/2025 - Camel formatted variable names; called package-specific functions; updated function for missingness with threshold; no longer set up to read in/out files from directory

#------------------------------------------------------------------------------#
# Load data
#------------------------------------------------------------------------------#

#' @export
dataTTEprocess <- function(inputFilePath, 
                           outputPath = NULL,
                             # Simulation parameters
                             nsims = NULL,
                             seed = NULL,
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
                             missingnessThreshold = 0.70){
  
  # Create output path if non-existent
  if(!dir.exists(outputPath)){
    dir.create(outputPath)
  }
  
  
  # Load the data 
  d <- readr::read_csv(inputFilePath) %>%
    rename(date_pcv1 = datePcv1,
           date_pcv2 = datePcv2,
           date_pcv3 = datePcv3,
           date_pcv4 = datePcv4,
           date_outcome = dateOutcome,
           date_admin_censor = dateAdminCensor)

  
  d$dob <- as.Date(d$dob, format = "%m/%d/%y") 
  d$date_pcv1 = as.Date(d$date_pcv1, format = "%m/%d/%y") 
  d$date_pcv2 = as.Date(d$date_pcv2, format = "%m/%d/%y") 
  d$date_pcv3 = as.Date(d$date_pcv3, format = "%m/%d/%y") 
  d$date_pcv4 = as.Date(d$date_pcv4, format = "%m/%d/%y")
  d$date_outcome = as.Date(d$date_outcome, format = "%m/%d/%y")
  d$date_admin_censor = as.Date(d$date_admin_censor, format = "%m/%d/%y")

   d <- d %>% 
    # Remove rows that do not have any data
    dplyr::filter(rowSums(is.na(.)) != ncol(.))
  
  #------------------------------------------------------------------------------#
  # Calculate the age (in days) at each event 
  #------------------------------------------------------------------------------#
  
  # Calculate the age (in days) each person received each PCV dose.
  # NOTE: These variables will be NA if a person received zero doses.
  d$dose1_age <- as.numeric(d$date_pcv1 - d$dob) # PCV dose 1
  d$dose2_age <- as.numeric(d$date_pcv2 - d$dob) # PCV dose 2
  d$dose3_age <- as.numeric(d$date_pcv3 - d$dob) # PCV dose 3
  d$dose4_age <- as.numeric(d$date_pcv4 - d$dob) # PCV dose 4
  
  # Calculate the age (in days) of the selected outcome
  d$outcome_age <- as.numeric(d$date_outcome - d$dob)
  
  # Calculate the age of drop out, which is either until the 5th birthday or 
  # administrative censoring (last day when each person showed up in the OHDSI
  # database).
  # NOTE: date_admin_censor in the dataset is either 5th birthday or the last date
  #       when each person showed up in the OHDSI database
  d$drop_age <- as.numeric(d$date_admin_censor - d$dob)
  
  # Define the last day of the follow up for those who were not censored or did 
  # not have an outcome
  # NOTE: We follow up each individual until the 5th birthday 
  followup_period <- round(365.25*5) # 5 years in days
  
  # Create empty object to store results
  out <- vector("list", 2)
  names(out) <- c("recommended","reduced")
  
  #------------------------------------------------------------------------------#
  # Data cleaning
  #------------------------------------------------------------------------------#
  
  # Exclude individuals with PCV records on the date of birth (DOB) (age 0 days) 
  # because this is data entry error. 
  d <- d[ which (
    !is.na(d$dose1_age) | d$dose1_age != 0 |   
      !is.na(d$dose2_age) | d$dose2_age != 0 | 
      !is.na(d$dose3_age) | d$dose3_age != 0 | 
      !is.na(d$dose4_age) | d$dose4_age != 0 ) ,  
  ]
  
  
  #------------------------------------------------------------------------------#
  # Define the intervals (in days) for the timing of each PCV dose under each of 
  # the compared dosing schedules
  #------------------------------------------------------------------------------#
  
  # This scripts compared 3+1 (recommended) vs. 1+1 (reduced) schedules
  
  #----------Recommended 3+1 Protocol----------#
  
  # Define the interval for the timing of PCV Dose 1
  # (between Day 38 and Day 92)
  recDose1Start <- recDose1Start # in days 
  recDose1End <- recDose1End # in days
  
  # Define the interval for the timing of PCV Dose 2
  recDose2Start <- recDose2Start
  recDose2End <- recDose2End
  
  # Define the interval for the timing of PCV Dose 3
  recDose3Start <- recDose3Start
  recDose3End <- recDose3End
  
  # Define the interval for the timing of PCV Dose 4 (Booster dose)
  recDose4Start <- recDose4Start
  recDose4End <- recDose4End
  
  #----------Reduced 1+1 Protocol----------#
  
  # Define the interval for the timing of PCV Dose 1
  reduDose1Start <- reduDose1Start
  reduDose1End <- reduDose1End
  
  # Define the interval for the timing of PCV Dose 2 (Booster dose)
  reduDose2Start <- reduDose2Start
  reduDose2End <- reduDose2End
  
  #------------------------------------------------------------------------------#
  # Create a formula for the Cox PH model to calculate the censoring weights 
  #------------------------------------------------------------------------------#
  
  # Formula for censoring weight 
  formulaSelect <- function(data, threshold = missingnessThreshold){
    
    # Throw a flag if there are a lot of missing values in your model.
    # we can modify to halt the process if this is an issue in the data
    if(length(which(is.na(data$sex)))/nrow(data) >= threshold | 
       length(which(is.na(data$race)))/nrow(data) >= threshold |
       length(which(is.na(data$ethnicity)))/nrow(data) >= threshold ){
      
      ParallelLogger::logInfo("Warning: some of the variables in this model are spare.")
    }
    
    else{
      
      # If the location of residence is NA for all individuals in the database
      # Or if this variable doesn't exist in the dataset
      # remove "state" from the model
      
      if(length(which(is.na((data$state)))) == nrow(data) | any(colnames(data) == "state") == FALSE) {
        fm_cn <- as.formula(paste("survival::Surv(surv_time, censored) ~ sex + race + ethnicity"))
      } else {
        fm_cn <- as.formula(paste("survival::Surv(surv_time, censored) ~ sex + race + ethnicity + state"))
      }
    }
    
    return(fm_cn)
  }
  
  
  # #------------------------------------------------------------------------------#
  # # Check how many individuals followed each of the compared dosing schedules in
  # # the database
  # # 
  # # NOTE: This section can be commented out
  # #------------------------------------------------------------------------------#
  # 
  # # Calculate the number of individuals that followed the recommended 3+1 schedule
  # sum(!is.na(d$outcome_age) &                 
  #       d$dose1_age >= recDose1Start & d$dose1_age <= recDose1End &     
  #       d$dose2_age >= recDose2Start & d$dose2_age <= recDose2End &
  #       d$dose3_age >= recDose3Start & d$dose3_age <= recDose3End &
  #       d$dose4_age >= recDose4Start & d$dose4_age <= recDose4End, na.rm=TRUE )      
  # # 47 in the fake data
  # 
  # # Calculate the number of individuals that followed the reduced 1+1 schedule
  # sum(!is.na(d$outcome_age) &                 
  #       d$dose1_age >= reduDose1Start & d$dose1_age <= reduDose1End &     
  #       d$dose2_age >= reduDose2Start & d$dose2_age <= reduDose2End, na.rm=TRUE )   
  # # 122 in the fake data
  
  #------------------------------------------------------------------------------#
  # Run the clone censor weight analysis
  #------------------------------------------------------------------------------#
  
  # Open data frames where we save clones created below for each of the 
  # compared protocols
  
  # Set the number of bootstrap re-sampling iterations to construct 95% CIs
  nsims <- nsims 
  pb <- utils::txtProgressBar(min = 1, max = nsims, style = 3)
  seed <- seed
  
  all_res_rec <- list()
  all_res_redu <- list()
  
  
  for(i in 1:nsims){
    
    # Run bootstrap re-sampling to construct 95% CIs by sampling rows with replacement
    set.seed(seed + i) # Because it is a random process, we should set a seed for reproducibility
    bootm <- d[sample(1:dim(d)[1], replace=TRUE), ] 
    
    #-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----# 
    # Create a clone for each individual under each of the compared dosing 
    # schedules for each set of bootstrap samples
    #-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----#
    
    #####----------Cloning for the Recommended 3+1 schedule----------#####
    
    # Create a new data frame for clones under the recommended 3+1 schedule
    df_rec <- bootm %>% 
      
      ### Step 1 for the recommended 3+1 schedule ###
      # Calculate the age (in days) of censoring "censoring_age" for each clone
      dplyr::mutate(censoring_age = case_when(
        
        #---Criteria 1---#
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before the follow-up period (5 years) ended; AND 
        # - an individual dropped out before they received Dose 1 (which is very less likely to happen) OR did not receive any PCV doses; AND 
        # - an individual dropped out before the last day of the Dose 1 time window. 
        drop_age < followup_period & 
          (drop_age < dose1_age | is.na(dose1_age)) & 
          drop_age < recDose1End ~ drop_age, 
        
        
        
        #---Criteria 2---#
        # For those who did not meet the former criteria, 
        # "censoring_age" should be "rec_dose1_end" if: 
        # - an individual didn't receive any PCV doses
        # They should be censored at the end of the time window for Dose 1 under this protocol
        is.na(dose1_age)  ~ recDose1End,
        
        #---Criteria 3---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose1_age" if: 
        # - an individual received Dose 1 before the first date of the time window for Dose 1 
        # They should be censored on the day they received Dose 1
        dose1_age < recDose1Start ~ dose1_age,
        
        #---Criteria 4---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose1_end" if: 
        # - an individual received Dose 1 after the last date of the time window for Dose 1
        # They should be censored on the last day of the time windoe for Dose 1 under the specified protocol
        dose1_age > recDose1End ~ recDose1End,
        
        #---Criteria 5---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1 during the correct time window; AND
        # - an individual dropped out before they received Dose 2 (which is less likely to happen) OR did not receive any other doses; AND
        # - an individual dropped out before the last day of the Dose 2 time window; AND
        # - an individual did not have an outcome
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose1_age) & 
          (dose1_age >= recDose1Start & dose1_age <= recDose1End) & 
          (drop_age < dose2_age | is.na(dose2_age)) &
          (drop_age < recDose2End) ~ drop_age, 
        
        #---Criteria 6---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose2_end" if: 
        # - an individual didn't receive Dose 2 
        # They should be censored on the last day of Dose 2 time window.
        is.na(dose2_age)  ~ recDose2End,
        
        #---Criteria 7---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose2_age" if: 
        # - an individual received Dose 2 within 28 days from Dose 1.
        # They should be censored when they received Dose 2. 
        (dose2_age - dose1_age < 28) ~ dose2_age,
        
        #---Criteria 8---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose2_age" if: 
        # - an individual received Dose 2 before the Dose 2 time window
        # They shoudl be censored when they received Dose 2.
        dose2_age < recDose2Start ~ dose2_age,
        
        #---Criteria 9---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose2_end" if: 
        # - an individual received Dose 2 after the Dose 2 time window 
        # They should be censored on the last day of the Dose 2 time window
        dose2_age > recDose2End ~ recDose2End, 
        
        #---Criteria 10---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1 and 2 during their correct time windows; AND
        # - an individual dropped out before they received Dose 3 (which is less likely to happen) OR did not receive any other doses; AND
        # - an individual dropped out before the last day of the Dose 3 time window; AND
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose2_age) & 
          (dose1_age >= recDose1Start & dose1_age <= recDose1End) & 
          (dose2_age >= recDose2Start & dose2_age <= recDose2End) &
          (drop_age < dose3_age | is.na(dose3_age)) &
          (drop_age < recDose3End) ~ drop_age,
        
        #---Criteria 11---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose3_end" if: 
        # - an individual didn't receive Dose 3 
        # They should be censored on the last day of the Dose 3 time window.
        is.na(dose3_age)  ~ recDose3End,
        
        #---Criteria 12---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose3_age" if: 
        # - an individual received Dose 3 within 28 days of Dose 2
        # They should be censored when they received Dose 3.
        (dose3_age - dose2_age < 28) ~ dose3_age,
        
        #---Criteria 13---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose3_age" if: 
        # - an individual received Dose 3 before the Dose 3 time window
        # They should be censored when they received Dose 3.
        dose3_age < recDose3Start ~ dose3_age,
        
        #---Criteria 14---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose3_end" if: 
        # - an individual received Dose 3 after the Dose 3 time window 
        # They should be censored on the last day of the Dose 3 time window.
        dose3_age > recDose3End ~ recDose3End, 
        
        #---Criteria 15---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1, 2, and 3 during their correct time windows; AND
        # - an individual dropped out before they received Dose 4 (which is less likely to happen) OR did not receive Dose 4; AND
        # - an individual dropped out before the last day of the Dose 4 time window; AND
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose3_age) & 
          (dose1_age >= recDose1Start & dose1_age <= recDose1End) & 
          (dose2_age >= recDose2Start & dose2_age <= recDose2End) &
          (dose3_age >= recDose3Start & dose3_age <= recDose3End) &
          (drop_age < dose4_age | is.na(dose4_age)) & 
          (drop_age < recDose4End) ~ drop_age, 
        
        #---Criteria 16---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose4_end" if: 
        # - an individual didn't receive Dose 4 
        # They should be censored on the last day of the Dose 4 time window.
        is.na(dose4_age)  ~ recDose4End,
        
        #---Criteria 17---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose4_age" if: 
        # - an individual received Dose 4  within 28 days of Dose 3 
        # They should be censored when they received Dose 4.
        (dose4_age - dose3_age < 28) ~ dose4_age,
        
        #---Criteria 18---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose4_age" if: 
        # - an individual received Dose 4 before the Dose 4 time window
        # They should be censored when they received Dose 4.
        dose4_age < recDose4Start ~ dose4_age,
        
        #---Criteria 19---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "rec_dose4_end" if: 
        # - an individual received Dose 4 after the Dose 4 time window
        # They should be censored on the last day of the Dose 4 time window.
        dose4_age > recDose4End ~ recDose4End, 
        
        #---Criteria 20---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1, 2, 3, and 4 during their correct time windows;
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose4_age) & 
          (dose1_age >= recDose1Start & dose1_age <= recDose1End) & 
          (dose2_age >= recDose2Start & dose2_age <= recDose2End) &
          (dose3_age >= recDose3Start & dose3_age <= recDose3End) &
          (dose4_age >= recDose4Start & dose4_age <= recDose4End) ~ drop_age, 
        
        #---Criteria 21---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "followup_period" if: 
        # - an individual received Dose 1, 2, 3, and 4 during their correct time windows and did not drop out
        # They should be followed up until the end.
        (dose1_age >= recDose1Start) & (dose1_age <= recDose1End) & 
          (dose2_age >= recDose2Start) & (dose2_age <= recDose2End) &
          (dose3_age >= recDose3Start) & (dose3_age <= recDose3End) &
          (dose4_age >= recDose4Start) & (dose4_age <= recDose4End)  ~ followup_period
      )
      )
    
    ### Step 2 for the recommended 3+1 schedule ###
    # Determine the infection outcome for the clone of each individual under the 
    # specified dosing schedule.
    df_rec$outcome_before_censoring <- ifelse( 
      
      # If an individual did not have an outcome, set outcome_before_censoring to 0.
      (df_rec$outcome==0), 0, 
      
      # Otherwise, outcome_before_censoring is 1 if the outcome occurred before 
      # an individual was censored
      ifelse(df_rec$outcome==1 & (df_rec$outcome_age < df_rec$censoring_age), 
             1, 
             0)) 
    
    ### Step 3 for the recommended 3+1 schedule ###
    # Determine the duration of follow up, "dur_followup", for the clone of each 
    # individual under the specific dosing schedule. 
    df_rec <- df_rec %>% 
      dplyr::mutate(dur_followup = case_when(
        
        # "dur_followup" should be "censoring_age" if:
        # the outcome occurred after censoring (likely due to non-adherence to the specified dosing schedule)
        outcome_before_censoring == 0 ~ censoring_age, 
        
        # "dur_followup" should be "outcome_age" if:
        # the outcome occurred before censoring 
        outcome_before_censoring == 1 ~ outcome_age, 
        
        # "dur_followup" should be "censoring_age" if:
        # an individual did not have an outcome
        outcome==0  ~ censoring_age 
        
      )
      )
    
    ### Step 4 for the recommended 3+1 schedule ###
    # Create an indicator variable, "censored", which is 1 if censored due to 
    # the protocol non-adherence, administrative drop off, or the end of study 
    # period; 0 otherwise.
    df_rec <- df_rec %>% dplyr::mutate(
      censored = case_when(
        
        # "censored" should be 0 if:
        # - an individual had the outcome before censoring
        outcome_before_censoring ==1 ~ 0,
        
        # "censored" should be 1 if:
        # - an individual had the outcome after censoring OR
        # - an individual who never had an outcome (outcome==0)
        outcome_before_censoring ==0 ~ 1 
        
      )
    )
    
    # Determine the survival period
    df_rec$surv_time <- df_rec$dur_followup ##### <-- Ask Shae. Do we need to create this new variable? If surv_time is equal to dur_followup, can we just use dur_followup?
    
    # Label protocol group
    df_rec$protocol <- "Recommended 3+1"
    
    #####----------Cloning for the Reduced 1+1 schedule----------#####
    
    # Create a new data frame for clones under the reduced 1+1 schedule
    df_redu <- bootm %>% 
      
      ### Step 1 for the reduced 1+1 schedule ###
      # Calculate the age (in days) of censoring "censoring_age" for each clone
      dplyr::mutate(censoring_age = case_when(
        
        #---Criteria 1---#
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before the follow-up period (5 years) ended; AND 
        # - an individual dropped out before they received Dose 1 (which is very less likely to happen) OR did not receive any PCV doses; AND 
        # - an individual dropped out before the last day of the Dose 1 time window. 
        drop_age < followup_period & 
          (drop_age < dose1_age | is.na(dose1_age)) & 
          drop_age < reduDose1End ~ drop_age, 
        
        #---Criteria 2---#
        # For those who did not meet the former criteria, 
        # "censoring_age" should be "reduDose1End" if: 
        # - an individual didn't receive any PCV doses
        # They should be censored at the end of the time window for Dose 1 under this protocol
        is.na(dose1_age)  ~ reduDose1End,
        
        #---Criteria 3---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose1_age" if: 
        # - an individual received Dose 1 before the first date of the time window for Dose 1 
        # They should be censored on the day they received Dose 1
        dose1_age < reduDose1Start ~ dose1_age,
        
        #---Criteria 4---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "reduDose1End" if: 
        # - an individual received Dose 1 after the last date of the time window for Dose 1
        # They should be censored on the last day of the time window for Dose 1 under the specified protocol
        dose1_age > reduDose1End ~ reduDose1End,
        
        #---Criteria 5---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1 during the correct time window; AND
        # - an individual dropped out before they received Dose 2 (which is less likely to happen) OR did not receive any other doses; AND
        # - an individual dropped out before the last day of the Dose 2 time window; 
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose1_age) & 
          (dose1_age >= reduDose1Start & dose1_age <= reduDose1End) &
          (drop_age < dose2_age | is.na(dose2_age)) &
          (drop_age < reduDose2End) ~ drop_age,
        
        #---Criteria 6---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "reduDose2End" if: 
        # - an individual didn't receive Dose 2 
        # They should be censored on the last day of Dose 2 time window.
        is.na(dose2_age)  ~ reduDose2End,
        
        #---Criteria 7---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose2_age" if: 
        # - an individual received Dose 2 within 28 days from Dose 1.
        # They should be censored when they received Dose 2. 
        (dose2_age - dose1_age < 28) ~ dose2_age,
        
        #---Criteria 8---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose2_age" if: 
        # - an individual received Dose 2 before the Dose 2 time window
        # They should be censored when they received Dose 2.
        dose2_age < reduDose2Start ~ dose2_age,
        
        #---Criteria 9---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "reduDose2End" if: 
        # - an individual received Dose 2 after the Dose 2 time window 
        # They should be censored on the last day of the Dose 2 time window
        dose2_age > reduDose2End ~ reduDose2End,
        
        #---Criteria 10---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "drop_age" if: 
        # - an individual dropped out before follow up period ended; AND
        # - an individual dropped out after they received Dose 1 and 2 during their correct time windows; AND
        # - an individual dropped out before they received Dose 3 (which is less likely to happen) OR did not receive any other doses;
        # They should be censored on the day they dropped out.
        drop_age < followup_period & 
          (drop_age > dose2_age) & 
          (dose1_age >= reduDose1Start & dose1_age <= reduDose1End) &
          (dose2_age >= reduDose2Start & dose2_age <= reduDose2End) &
          (drop_age < dose3_age | is.na(dose3_age)) ~ drop_age,
        
        #---Criteria 11---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "dose3_age" if: 
        # - an individual received Dose 3 
        # They should be censored on the day they received Dose 3
        !is.na(dose3_age) ~ dose3_age, 
        
        #---Criteria XX---#
        # Got fourth dose - censor when they got dose 4- should not happen, but 
        # could occur from data entry errors where dose 3 is NA but not dose 4
        
        #---Criteria 12---#
        # For those who did not meet any of the former criteria, 
        # "censoring_age" should be "followup_period" if: 
        # - an individual received Dose 1 and 2 during their correct time windows; AND  
        # - an individual did not receive Dose 3 and 4
        # They should be followed up until the end.
        (dose1_age >= reduDose1Start) & (dose1_age <= reduDose1End) & 
          (dose2_age >= reduDose2Start) & (dose2_age <= reduDose2End) &
          is.na(dose3_age) & 
          is.na(dose4_age) ~ followup_period
      )
      )
    
    ### Step 2 for the reduced 1+1 schedule ###
    # Determine the infection outcome for the clone of each individual under the 
    # specified dosing schedule.
    df_redu$outcome_before_censoring <- ifelse(
      
      # If an individual did not have an outcome, set outcome_before_censoring to 0.
      (df_redu$outcome==0), 0, 
      
      # Otherwise, outcome_before_censoring is 1 if the outcome occurred before 
      # an individual was censored
      ifelse(df_redu$outcome==1 & (df_redu$outcome_age < df_redu$censoring_age), 
             1, 
             0)) 
    
    ### Step 3 for the reduced 1+1 schedule ###
    # Determine the duration of follow up, "dur_followup", for the clone of each 
    # individual under the specific dosing schedule. 
    df_redu <- df_redu %>% 
      dplyr::mutate(dur_followup = case_when(
        
        # "dur_followup" should be "censoring_age" if:
        # the outcome occurred after censoring (likely due to non-adherence to the specified dosing schedule)
        outcome_before_censoring == 0 ~ censoring_age, 
        
        # "dur_followup" should be "outcome_age" if:
        # the outcome occurred before censoring 
        outcome_before_censoring == 1 ~ outcome_age, 
        
        # "dur_followup" should be "censoring_age" if:
        # an individual did not have an outcome
        outcome==0  ~ censoring_age 
        
      )
      )
    
    ### Step 4 for the reduced 1+1 schedule ###
    # Create an indicator variable, "censored", which is 1 if censored due to 
    # the protocol non-adherence, administrative drop off, or the end of study 
    # period; 0 otherwise.
    df_redu <- df_redu %>% dplyr::mutate(
      censored = case_when(
        
        # "censored" should be 0 if:
        # - an individual had the outcome before censoring
        outcome_before_censoring ==1 ~ 0,
        
        # "censored" should be 1 if:
        # - an individual had the outcome after censoring
        outcome_before_censoring ==0 ~ 1 
        
      )
    )
    
    # Determine the survival period
    df_redu$surv_time <- df_redu$dur_followup 
    
    # Label protocol group
    df_redu$protocol <- "Reduced 1+1"
    
    
    #####----------Create an analytic dataset for clones----------#####
    ## We can likely store these datasets in a list to apply transformations more efficiently 
    ## Even though this is more processing, it's fewer datasets in memory
    
    # Merge cloned datasets for all compared protocols
    #df_protocol_all <- rbind(df_rec, df_redu)  
    #out <- list(df_rec, df_redu)
    
    # Define the outcome outcome at dur_followup under each protocol.
    # 1 if a clone had an outcome and their outcome was observed under each protocol.
    # 0 otherwise.
    df_rec <- df_rec %>% dplyr::mutate(clone.outcome = case_when(
      outcome==0                    ~ 0, # No infection in the real world
      outcome_before_censoring == 1 ~ 1, # Had infection before being censored - counted
      outcome_before_censoring == 0 ~ 0, # Had infection after being censored - not counted
      TRUE ~ 0 # Others
    ))
    df_redu <- df_redu %>% dplyr::mutate(clone.outcome = case_when(
      outcome==0                    ~ 0, # No infection in the real world
      outcome_before_censoring == 1 ~ 1, # Had infection before being censored - counted
      outcome_before_censoring == 0 ~ 0, # Had infection after being censored - not counted
      TRUE ~ 0 # Others
    ))
    
    # Adjust data type for each variable (e.g., numeric, factor)
    continuous_list <- c("dose1_age", "dose2_age", "dose3_age", "dose4_age", "surv_time", 
                         "dur_followup", "outcome_age", "drop_age", "clone.outcome")
    categorical_list <- c("sex", "race", "ethnicity", "state", "protocol")
    
    df_rec[continuous_list] <- sapply(df_rec[continuous_list], as.numeric)
    df_rec[categorical_list] <- sapply(df_rec[categorical_list], as.factor) 
    
    df_redu[continuous_list] <- sapply(df_redu[continuous_list], as.numeric)
    df_redu[categorical_list] <- sapply(df_redu[categorical_list], as.factor) 
    
    
    #-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----#
    # Compute the inverse probability of censoring weight (IPCW) for each clone
    # under each of the compared protocols
    #-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----#
    
    #####----------Recommended 3+1 schedule----------#####
    
    # Select the subset for the clones created for the recommended 3+1 schedule 
    #rec_protocol <- df_protocol_all %>% subset(protocol == "Recommended 3+1") 
    
    
    # Fit a Cox proportional hazards (PH) model for censoring
    # (the model outcome is being censored.)
    fm_cn <- formulaSelect(df_rec)
    coxph.censor <- survival::coxph(fm_cn, data = df_rec)
    
    # Subset the clone population to just those who had the outcome
    # because those who did not have the outcome do not contribute to the calculation
    # of the cumulative risk of infection.
    cases <- df_rec[df_rec$clone.outcome==1,] 
    
    # Predict the probability of remaining uncensored at each person’s event time
    # (date of the selected outcome), using the survival package with broom.
    cases_rec <- broom::augment(coxph.censor, 
                                newdata=cases, 
                                type.predict = "expected") %>%
      dplyr::mutate(prob = exp(-.fitted))
    
    # Order the subset data by the event time 
    # (i.e., number of days between the index date and the date of outcome)
    cases_rec <- cases_rec[order(cases_rec$surv_time),]
    
    # Compute the cumulative sum of 1/predictions at each of the times, which is
    # our cumulative incidence curve
    cases_rec$wt <- 1/cases_rec$prob
    
    # Create time data 
    cases_rec <- cases_rec %>% 
      subset(., select = c(pid, wt, surv_time, protocol))
    cases_rec <- data.frame(surv_time=seq(1, followup_period, 1)) %>% 
      left_join(., cases_rec, by = "surv_time")
    
    # Compute the weighted risk of the outcome for each clone that had the outcome
    cases_rec$risk <- cumsum(tidyr::replace_na(cases_rec$wt, 0)) / nrow(df_rec)
    
    res_rec <- cases_rec[which(cases_rec$surv_time <= followup_period),] %>%
      dplyr::group_by(surv_time) %>%
      dplyr::summarise(cumrisk = max(risk, na.rm=TRUE))
    
    # Add back in protocol column
    res_rec$protocol <- "Recommended 3+1"
    
    # Note iteration 
    res_rec$sim <- i
    
    # Extract the maximum risk (i.e., cumulative weighted risk of the outcome) on each event day
    all_res_rec[[i]] <- res_rec
    
    
    #####----------Recommended 3+1 schedule----------#####
    
    # Select the subset for the clones created for the reduced 1+1 schedule 
    #redu_protocol <- df_protocol_all %>% subset(protocol == "Reduced 1+1") ##### <-- Ask Shae. Was it needed to create df_protocol_all if we need to work on df_rec and df_redu separately here?
    
    # Fit a Cox proportional hazards (PH) model for censoring
    # (the model outcome is being censored.)
    fm_cn <- formulaSelect(df_redu)
    coxph.censor <- survival::coxph(fm_cn, data = df_redu)
    
    # Subset the clone population to just those who had the outcome
    # because those who did not have the outcome do not contribute to the calculation
    # of the cumulative risk of infection.
    cases <- df_redu[df_redu$clone.outcome ==1,]
    
    # Predict the probability of remaining uncensored at each person’s event time
    # (date of the selected outcome), using the survival package with broom.
    cases_redu <- broom::augment(coxph.censor,
                                 newdata=cases,
                                 type.predict = "expected") %>%
      dplyr::mutate(prob = exp(-.fitted))
    
    # Order the subset data by the event time 
    # (i.e., number of days between the index date and the date of outcome)
    cases_redu <- cases_redu[order(cases_redu$surv_time),]
    
    # Compute the cumulative sum of 1/predictions at each of the times, which is
    # our cumulative incidence curve
    cases_redu$wt <- 1/cases_redu$prob
    
    # Create time data
    cases_redu <- cases_redu %>%
      subset(., select = c(pid, wt, surv_time))
    cases_redu <- data.frame(surv_time=seq(1, followup_period, 1)) %>%
      left_join(., cases_redu, by = "surv_time")
    
    # Compute the weighted risk of the outcome for each clone that had the outcome
    cases_redu$risk <- cumsum(tidyr::replace_na(cases_redu$wt, 0)) / nrow(df_redu)
    
    
    res_redu <- cases_redu[which(cases_redu$surv_time <= followup_period),] %>%
      dplyr::group_by(surv_time) %>%
      dplyr::summarise(cumrisk = max(risk, na.rm=TRUE))
    
    # Add back in protocol column
    res_redu$protocol <- "Reduced 1+1"
    
    # Note iteration 
    res_redu$sim <- i
    
    # Extract the maximum risk (i.e., cumulative weighted risk of the outcome) on each event day
    all_res_redu[[i]] <- res_redu

    
    setTxtProgressBar(pb, i)

  }
  
  close(pb)
  ParallelLogger::logInfo("Analytic datasets and IPCW output created.")
  #------------------------------------------------------------------------------#
  # Save output files of the clone censor weight analysis
  #------------------------------------------------------------------------------#
  
  
  #------------------------------------------------------------------------------#
  # Explore the created clone datasets to make sure that the code is working 
  #------------------------------------------------------------------------------#
  
  # Check results, only look at these columns for simplicity 
  #d_filtered <- df_protocol_all[, c("pid", "protocol", 
  #                                  "dose1_age", "dose2_age", "dose3_age", "dose4_age", 
  #                                  "drop_age", "censoring_age", "censored", "dur_followup", "surv_time", 
  #                                  "outcome_age", "outcome", "clone.outcome", "outcome_before_censoring")]
  
  # Check results for random rows
  #random_rows <- d_filtered %>% sample_n(50)
  
  
  rm(df_rec)
  rm(df_redu)
  
  # Export results to local folder 
  if(!is.null(outputPath)){
    # get the base file name ("outcome_xxx")
    basefile <- basename(inputFilePath)
    outcomeString = stringr::str_extract(basefile, "(?<=cohorts_)[^.]+")
    
    # combine the recommended schedule and reduced schedule results into two big data frames
    resRec = dplyr::bind_rows(all_res_rec)
    resReduced = dplyr::bind_rows(all_res_redu)
    
    # and save them as CSV files 
    outputFilePathRec = file.path(outputPath, paste0(outcomeString, "_recommended.csv"))
    readr::write_csv(resRec, outputFilePathRec)
    
    outputFilePathReduced = file.path(outputPath, paste0(outcomeString, "_reduced.csv"))
    readr::write_csv(resReduced, outputFilePathReduced)
    
    # clean up
    rm(res_rec)
    rm(res_redu)
    rm(resRec)
    rm(resReduced)
    gc() 
    
    mes = paste("Finished TTE analysis for", outcomeString, "and saved results.")
    ParallelLogger::logInfo(mes)
  }else{
    out[[1]] <- all_res_rec
    out[[2]] <- all_res_redu
    
    rm(res_rec)
    rm(res_redu)
    gc() 
    
    return(out)
  }
}


#' @export
runTTEBatch <- function(cohortPath, 
                        outputPath = NULL, 
                        nsims, 
                        seed){
  
  if(is.null(outputPath)){
    stop("For batch TTE run, must specify `outputPath`!")
  }
  
  # Create output path if non-existent
  if(!dir.exists(outputPath)){
    dir.create(outputPath)
  }
  
  # list all cohort files
  allCohortFiles = list.files(cohortPath, full.names = TRUE)
  
  # run through them 
  for(cf in allCohortFiles){
    dataTTEprocess(inputFilePath = cf, outputPath = outputPath, nsims = nsims, seed = seed)
  }
}
