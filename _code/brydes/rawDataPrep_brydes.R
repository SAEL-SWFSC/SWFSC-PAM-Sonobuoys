# Raw Data Prep

# This script should only be run when there is *new* data and analysis to conduct.

library(here)
here()
source(here::here("_code/brydes/_brydesCommonR.R"))

### DEPLOY ###
source(here("_code/merge_sonobuoy_csv.R"))
survey_data <- merge_sonobuoy_csv(input_dir = here("data/surveyData"), output_file = here("data/surveyData.csv"))

sightings_path = "data/brydes/sightings.csv"
deploy_df = read.csv(here("data/surveyData.csv"))
output_path = "data/brydes/deploy.rds"

deploy <- prep_deploy_data(
  sightings_path, 
  deploy_df,
  output_path,
  species_ids = c("72", "99"),
  strict_check = FALSE #does a strict check and fails if ANY deploy rows do not have an associated sightings row
)

## TEMP TESTING OF DATES
#source(here("_code/compare_dates.R"))

# Basic call (assumes column is named "date")
#result <- compare_dates(deploy, survey_data)
#result
# With a custom column name and meaningful labels
# result <- compare_dates(df1, df2,
#                         date_col = "Date",
#                         df1_name = "cruise_1631",
#                         df2_name = "cruise_1616")

### REVIEW ###

review <- prep_manual_review(
  review_path = "data/swfsc_sonobuoy_manualReview.csv",
  output_path = "data/brydes/review.rds"
)


### RAVEN ###
source(here::here("_code/brydes/raven_selection_tables.R"))

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
