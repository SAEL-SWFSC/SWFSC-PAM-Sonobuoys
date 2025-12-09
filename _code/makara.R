## Formatting Makara table templates
#Things to check:
# If Buoy is lost, then no values for anything

## Prep
library(tidyverse)
library(here)
organization_code <- "SWFSC"
device_type <- "DRIFTING_BUOY"
deployment_platform_type_code <- "SONOBUOY"
code_list <- c("53", "53A", "53B", "53D", "53F", "57", "57A", "77", "77A", "77C")
recording_sample_rate_kHz <- "48"
recording_bit_depth <- "16"  #Check if this is true for all?
recording_n_channel <- "2"
recording_filetypes <- "WAV"
recording_timezone <-"Local"
recording_device_depth_m <-   #
recording_redacted <- "FALSE" 
analysis_granularity_code <- "Event_CALL"
analysis_processing_code <- "POST_PROCESSED"
analysis_quality_code <- FULLY_VALIDATED
analysis_dataset_url <-  #URL for data in Github Repo
analysis_release_data <- TRUE
analysis_release_pacm <- TRUE
detector_codes <- "MANUAL"
analysis_organization_code <- "SWFSC"
analysis_analysts <- "SRANKIN"

#################################################
#####  Merge Sonobuoy Data from All Surveys #####
#################################################

library(tidyverse)
library(lubridate)

#-------------------------------------------------------------
# 1. List all CSV files
#-------------------------------------------------------------
files <- list.files(here("_data/surveyData"), pattern = "\\.csv$", full.names = TRUE)

#-------------------------------------------------------------
# 2. Define column types you want
#-------------------------------------------------------------
char_cols <- c("Sonobuoy Type", "Cruise", "Depth", "Sounds Detected",
               "Additional Tapes?", "Recording Reviewed?", "Comments")

num_cols  <- c("Latitude", "Longitude", "Sonobuoy Number", "Sighting Number",
               "Channel", "Hours to Scuttle", "Bad Buoys", "Ship Noise?",
               "Difar Signal?", "Water Noise?", "Biological Sounds?",
               "DAT Tape Number", "Reception Distance (nmi)")

#-------------------------------------------------------------
# 3. Function to read + clean each file
#-------------------------------------------------------------
read_and_clean <- function(path) {
  
  # Extract filename only
  fname <- basename(path)
  
  # Extract year and cruise name
  # Filename format: Sonobuoys_YYYY_CruiseName.csv
  year   <- str_extract(fname, "(?<=Sonobuoys_)\\d{4}(?=_)")
  cruise <- str_extract(fname, "(?<=\\d{4}_).+(?=\\.csv)")
  
  df <- read_csv(path, show_col_types = FALSE) %>%
    mutate(
      # Add year and cruise columns
      year = year,
      cruise_name = cruise
    )
  
  #-----------------------------------------------------------
  # 4. Fix Date column — allow multiple input formats
  #-----------------------------------------------------------
  # Accepts formats like:
  #   "2020-01-05 13:20", "01/05/2020 1:20 PM", "2020/01/05", etc.
  df <- df %>%
    mutate(
      Date = parse_date_time(
        Date,
        orders = c("Ymd HMS", "Ymd HM", "Ymd",
                   "mdY HMS", "mdY HM", "mdY",
                   "dmY HMS", "dmY HM", "dmY")
      )
    )
  
  #-----------------------------------------------------------
  # 5. Convert column classes
  #-----------------------------------------------------------
  # Convert character columns
  for (col in char_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  
  # Convert numeric columns
  for (col in num_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }
  
  df
}

#-------------------------------------------------------------
# 6. Apply function to all files and merge into one tibble
#-------------------------------------------------------------
surveyData <- files %>%
  map_dfr(read_and_clean)

#-------------------------------------------------------------
# 7. Final formatting of Date column to standard output format
#-------------------------------------------------------------
surveyData <- surveyData %>%
  mutate(Date = format(Date, "%Y-%m-%d %H:%M"))

#-------------------------------------------------------------
# 8. Save as a csv file in the _data folder
#-------------------------------------------------------------
write_csv(surveyData, here("_data/surveyData.csv"))


################################## 
#####  Modify Deploy Details #####
#################################
# Deployments
myData <- data <- read_csv("your_file.csv") %>%
  rename_with(~ gsub(" ", "_", .), everything())  # Read in your project file
project_region <-    # name of project region, ex "CCE"
deployment_cruise <- "YYYY_CRUISENAME" #Name of cruise with year, ex "2005_PICEAS"
deployment_comments <-  #IF associated with a sighting, code in this info

  

  
# Recordings
recording_device <- # get myData$Sonobuoy_Type and then use grepl to ensure it is all caps, no spaces, etc

# Analyses

analysis_type_URL <- "CHOOSE" # Identify URL for methods description to put in comments
analysis_sound_source_codes <- c("BLWH", "SEWH", "FIWH", "HUWH", "MIWH", "NPRW", "SPWH")

  
  
############################  
#####  Info to define  #####
############################

  # Note: Once I define these, determine their order based on their subcomponents

project_code <- paste(deployment_cruise, deployment_platform_type_code, myData$Sonobuoy_Number, sep = "_")    # project code, ex "PICEAS_2005_SB_001"
  
#site_code <-   #NOT NECESSARY FOR MOBILE PLATFORMS
  
deployment_code <- paste(organization_code, project_code, sep = "_") #unique code for deployment
  
deployment_datetime <- myData$Date #Date and time of start of deployment

deployment_latitude <- myData$Latitude #in Decimal Degrees
  
deployment_longitude <- myData$Longitude #in Decimal Degrees  

recovery_datetime <- deployment_datetime #Not appropriate for sonobuoys, add as comment
recovery_latitude <- deployment_latitude  #Not appropriate for sonobuoys, add as comment
recovery_longitude <- deployment_longitude  #Not appropriate for sonobuoys, add as comment
recovery_comments <- "Sonobuoys are expendable and we use the deployment information for the recovery_datetime, 
                     # recovery_latitude, and recovery_longitude"
parent_deployment_code <- deployment_cruise


# Recordings
recording_code <- paste(project_code, "_RECORDING") #check, may need to change this?
recording_codes <- recording_code
recording_device_codes <- paste(deployment_platform_type_code, myData$Sonobuoy_Type, , sep = "_")
recording_start_datetime <- myData$Date #Date and time of start of deployment, required for all devices where recording_device_lost is FALSE
recording_usable_start_datetime <- myData$recording_usable_start_datetime
recording_usable_end_datetime <- myData$recording_usable_end_datetime
recording_end_datetime <- myData$recording_usable_end_datetime   #Need to identify end of recording, required for all devices where recording_device_lost is FALSE
recording_duration_secs <- myData$recording_duration_secs
recording_interval_secs <-recording_duration_secs
recording_channel <- myData$recording_channel
recording_usable_max_frequency_kHz <-  #If/then statement depending on type of buoy deployed
recording_n_channel <- myData$recording_n_channel
recording_channel <- 2 # Need to read up on this more. Required for NCEI
  
recording_quality_code <- myData$recording_quality   #"COMPROMISED", "GOOD", "UNUSABLE", "UNVERIFIED"

recording_device_lost <- myData$bad_buoy  #FALSE means good buoy, TRUE means bad buoy

recording_uri <- paste("gs://swfsc-1", deployment_cruise, "sonobuoys", sep = "/") #Uniform Resource Identifier (URI) for detector output

recording_comments<-  #add info that for 2 Ch Recordings; Recording times were local to the ship (varied) and aligned with the visual
  # sighting datasets. Depth of 90ft was triggered, but depth may have been compromised by buoy modifications (cite paper). 
  # For sonobuoys, often the same buoy was recorded on both. Value for recording_n_channels identifies the # of unique channels; 
  
 # Analysis
analysis_type <- paste(myData$analysis_type, "kHz", sep="") # Identify types of analyses and give URL for the methods description. "250", "48k", etc
analysis_code <- paste(organization_code, deployment_platform_type_code, analysis_type, sep = "_")
analysis_start_datetime <- myData$analysis_start_datetime
analysis_end_datetime <- myData$analysis_end_datetime
analysis_min_frequency_kHz <- 0
analysis_max_frequency_kHz <- 0.5*(myData$analysis_type*1000) #This is not quite right, but fix this
analysis_protocol_reference <-  #Citation with DOI, 
analysis_comments <-myData$analysis_comments


### DETECTIONS
detection_start_datetime <-  # get this from raven selection table
detection_end_datetime <- # get this from raven selection table
detection_effort_secs <- #from raven selection table-- sum the 'on effort' times for that analysis, then convert to seconds
detection_call_type_code <- #use their codes
detection_restuls_code <- #need to think about this, choices are "DETECTED" "POSSIBLY_DETECTED", "NOT_DETECTED". tHIS DOESN'T MAKE SENSE HERE?


# Create *.csv with info in correct order, name, etc

