# Raw Data Prep

# This script should only be run when there is *new* data and analysis to conduct.

library(here)
here()
source(here::here("_code/_brydesCommonR.R"))

# Only run if needed

### DEPLOY ###
survey_data <- combine_survey_data(keep_date_raw = FALSE) #Combine survey data all *.csv files in data/surveyData folder

sightings_path = "data/brydes/sightings.csv"
deploy_path = read.csv(here("data/surveyData.csv"))
output_path = "data/brydes/deploy.rds"

deploy <- prep_deploy_data(
  sightings_path, 
  deploy_path,
  output_path,
  species_ids = "72"
)

### REVIEW ###

review <- prep_manual_review(
  review_path = "data/swfsc_sonobuoy_manualReview.csv",
  output_path = "data/brydes/review.rds"
)


### RAVEN ###
source(here::here("_code/raven_selection_tables.R"))

raven_results <- validate_merge_raven(
  folder_path    = here("brydesData/"),
  merge          = TRUE,
  merge_on_errors = TRUE,         # set FALSE to merge only clean files
  output_file    = here("data/brydes", "raven.rds")
)

# Optionally save validation errors for review
if (raven_results$has_errors) {
  write_csv(results$errors,  "validation_errors.csv")
  write_csv(results$summary, "validation_summary.csv")
}

raven <- raven_results$data

### RAVEN EFFORT ###
source(here::here("_code/raven_effort_analysis.R"))
raven_effort <- process_effort_analysis(
  folder_path = here("brydesData/"),
  output_file = here("data/brydes", "raven_effort.rds")
)
