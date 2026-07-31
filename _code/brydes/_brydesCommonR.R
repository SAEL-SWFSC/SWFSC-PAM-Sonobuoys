# Load Library
library(here)
library(tidyverse)
library(tidyr)
library(purrr)
library(lubridate)
library(janitor)
library(warbleR)
library(Rraven)
library(dplyr)
library(stringr)
library(readr)
library(clipr)
library(sf)
library(marmap)
library(ggspatial)
library(gt)
library(webshot2)
library(patchwork)
library(swfscDAS)
library(lubridate)
library(ggplot2)
library(flextable)
library(extrafont)
# font_import(pattern = "Times")  # one-time, slow
# loadfonts(device = "win")  # or "pdf"/"postscript" depending on OS

# Source Other Files
source(here::here("_code/brydes/diurnal_polar_plot.R"))

# =============================================================================
# DataPrep: Add Sightings to Deploy
# =============================================================================
prep_deploy_data <- function(sightings_path, deploy_df, species_ids,
                             output_path = NULL,
                             strict_check = TRUE,
                             # Must match OUTPUT_DATE_FORMAT in merge_sonobuoy_csv.R.
                             # deploy_df$date is expected to already be standardized
                             # to this single format by that upstream script.
                             date_format = "%m/%d/%Y %H:%M") {
  
  # 1. Load sightings from path; deploy is already a dataframe
  sightings_raw <- read_csv(here(sightings_path), show_col_types = FALSE) %>%
    clean_names()
  
  # 2. Clean and format deploy
  n_na_before <- sum(is.na(deploy_df$date))
  
  deploy_clean <- deploy_df %>%
    mutate(
      cruise          = as.character(cruise),
      sonobuoy_number = as.character(sonobuoy_number),
      dat_number      = as.character(dat_tape_number),
      # tz = "UTC" is a neutral label for floating local times only —
      # no timezone conversion happens; it just avoids Sys.timezone().
      date            = as.POSIXct(date, format = date_format, tz = "UTC")
    )
  
  n_na_after <- sum(is.na(deploy_clean$date))
  
  if (n_na_after > n_na_before) {
    stop(
      "prep_deploy_data(): date parsing introduced ", n_na_after - n_na_before,
      " new NA value(s) in `date`. This means some values in deploy_df$date ",
      "don't match date_format = '", date_format, "'. Since deploy_df$date is ",
      "expected to already be standardized by merge_sonobuoy_csv.R's ",
      "OUTPUT_DATE_FORMAT, check whether that format changed, or whether ",
      "deploy_df was built some other way.",
      call. = FALSE
    )
  }
  
  # 3. Clean and format sightings
  sightings_clean <- sightings_raw %>%
    mutate(
      cruise           = as.character(cruise),
      sonobuoy_number  = as.character(sonobuoy_number),
      # ASSUMPTION: sightings has a column that becomes `dat_number` after
      # clean_names(). If it's actually named `dat_tape_number` (like deploy's
      # raw column), change the right-hand side below to
      # as.character(dat_tape_number). Verify against your actual column names.
      dat_number       = as.character(dat_number),
      sighting_species = as.character(sighting_species),
      near_species     = as.character(near_species)
    )
  
  # 3.5 CHECK: every deploy row must have a matching sightings row,
  #     matched on cruise + sonobuoy_number + dat_number. It's fine for
  #     sightings to have combos that aren't in deploy — only the reverse
  #     direction is checked.
  #
  #     strict_check = TRUE  (default): stop() and halt execution
  #     strict_check = FALSE: warning() and continue (data will carry NAs
  #                            for the missing combos downstream)
  deploy_keys <- deploy_clean %>%
    select(cruise, sonobuoy_number, dat_number) %>%
    distinct()
  
  sightings_keys <- sightings_clean %>%
    select(cruise, sonobuoy_number, dat_number) %>%
    distinct()
  
  missing_in_sightings <- anti_join(
    deploy_keys, sightings_keys,
    by = c("cruise", "sonobuoy_number", "dat_number")
  )
  
  if (nrow(missing_in_sightings) > 0) {
    missing_list <- missing_in_sightings %>%
      mutate(
        combo = paste0(
          "cruise=", cruise,
          ", sonobuoy_number=", sonobuoy_number,
          ", dat_number=", dat_number
        )
      ) %>%
      pull(combo)
    
    msg <- paste0(
      "prep_deploy_data(): ", nrow(missing_in_sightings),
      " deploy row(s) have no matching entry in sightings ",
      "(matched on cruise + sonobuoy_number + dat_number):\n",
      paste0("  - ", missing_list, collapse = "\n")
    )
    
    if (strict_check) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
  
  # 4. Get all cruise/sonobuoy combos present in sightings (for NA flagging)
  #    NOTE: this stays on 2 keys (cruise/sonobuoy_number) for the species
  #    loop below, which is about linking a *species observation* to a
  #    deployment record, not the same thing as the existence check above.
  #    When strict_check = TRUE, step 3.5 guarantees every deploy combo has
  #    a matching sightings combo, so the is.na(.in_sightings) branch below
  #    is a redundant safety net. When strict_check = FALSE, that branch is
  #    what actually produces the NAs for any combos flagged as missing.
  sightings_combos <- sightings_clean %>%
    select(cruise, sonobuoy_number) %>%
    distinct() %>%
    mutate(.in_sightings = TRUE)
  
  # 5. Loop over species_ids with purrr::reduce, adding one column per species
  processed_deploy <- purrr::reduce(
    as.character(species_ids),
    .init = deploy_clean,
    function(current_df, sp_id) {
      
      new_col_name <- paste0(sp_id, "_sightings")
      
      species_matches <- sightings_clean %>%
        filter(sighting_species == sp_id | near_species == sp_id) %>%
        select(cruise, sonobuoy_number) %>%
        distinct() %>%
        mutate(!!new_col_name := TRUE)
      
      current_df %>%
        left_join(species_matches, by = c("cruise", "sonobuoy_number")) %>%
        left_join(sightings_combos, by = c("cruise", "sonobuoy_number")) %>%
        mutate(
          !!new_col_name := case_when(
            is.na(.in_sightings)          ~ NA,
            .data[[new_col_name]] == TRUE ~ TRUE,
            TRUE                          ~ FALSE
          )
        ) %>%
        select(-.in_sightings)
    }
  )
  
  # 6. Optionally save to .rds
  if (!is.null(output_path)) {
    saveRDS(processed_deploy, file = here(output_path))
    message("Output saved to: ", here(output_path))
  }
  
  return(processed_deploy)
}
# =============================================================================
# DataPrep: Manual Review
# =============================================================================
prep_manual_review <- function(review_path = "data/swfsc_sonobuoy_manualReview.csv", 
                               output_path = "data/brydes/review.rds") {
  
  # 1. Load data safely using here() and clean initial names
  review_raw <- read_csv(here(review_path), show_col_types = FALSE) %>% 
    clean_names()
  
  # 2. Process data: rename, cast type, and filter
  review <- review_raw %>%
    rename(sonobuoy_number = deployment_id,
           cruise = cruise_number) %>%
    mutate(sonobuoy_number = as.numeric(sonobuoy_number),
           dat_number = str_remove(folder_name, ".*SBDAT") %>% as.numeric()
           ) %>%
    filter(recording_quality_code != "UNUSABLE")
  
  # 3. Ensure the output directory exists before saving (defensive programming)
  output_dir <- dirname(here(output_path))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 4. Save as .rds
  saveRDS(review, file = here(output_path))
  
  # Return the data frame invisibly in case you want to pipe it directly into something else
  return(review)
}
# =============================================================================
# DataPrep: Count True Annotations & Merge to Review dataframe
# =============================================================================
count_true_annotations <- function(review, raven, annotation_col) {
  
  true_values <- c("Y", "Yes", "T", "TRUE")
  
  raven_summary <- raven %>%
    mutate(across(c(cruise, dat_number), as.character)) %>%   # <-- coerce
    group_by(cruise, dat_number) %>%
    summarise(
      n_true       = sum(.data[[annotation_col]] %in% true_values, na.rm = TRUE),
      n_total_rows = n(),
      .groups = "drop"
    )
  
  review %>%
    mutate(across(c(cruise, dat_number), as.character)) %>%   # <-- coerce
    left_join(raven_summary, by = c("cruise", "dat_number")) %>%
    mutate(
      !!annotation_col := case_when(
        is.na(n_total_rows) ~ NA_integer_,
        TRUE                ~ as.integer(n_true)
      )
    ) %>%
    select(-n_true, -n_total_rows)
}
# =============================================================================
# DataPrep: Merge Deploy & Review
# =============================================================================
merge_deploy_review <- function(deploy_df, review_df, remove_unmatched_deploy = FALSE) {
  
  # 0. Coerce cruise and dat_number to character in both dataframes
  deploy_df <- deploy_df %>% mutate(across(c(cruise, dat_number), as.character))
  review_df <- review_df %>% mutate(across(c(cruise, dat_number), as.character))
  
  # 1. Check for rows in review that do not exist in deploy
  unmatched_reviews <- anti_join(review_df, deploy_df, by = c("cruise", "dat_number"))
  
  if (nrow(unmatched_reviews) > 0) {
    examples <- unmatched_reviews %>% 
      select(cruise, dat_number) %>% 
      distinct() %>% 
      head(5)
    
    example_string <- paste(capture.output(print(examples)), collapse = "\n")
    
    warning(paste0(
      "Found ", nrow(unmatched_reviews), " row(s) in 'review' that do not exist in 'deploy'!\n",
      "Showing the first few unmatched pairs:\n",
      example_string
    ))
  }
  
  # 2. Merge dataframes based on user preference
  if (remove_unmatched_deploy) {
    merged_df <- inner_join(deploy_df, review_df, by = c("cruise", "dat_number"))
  } else {
    merged_df <- left_join(deploy_df, review_df, by = c("cruise", "dat_number"))
  }
  
  return(merged_df)
}

# 
# merge_deploy_review <- function(deploy_df, review_df, remove_unmatched_deploy = FALSE) {
#   
#   # 1. Check for rows in review that do not exist in deploy
#   # anti_join finds rows in review that have no match in deploy based on cruise & dat_number
#   unmatched_reviews <- anti_join(review_df, deploy_df, by = c("cruise", "dat_number"))
#   
#   if (nrow(unmatched_reviews) > 0) {
#     # Isolate the unique combinations of unmatched cruise and dat_numbers
#     examples <- unmatched_reviews %>% 
#       select(cruise, dat_number) %>% 
#       distinct() %>% 
#       head(5)
#     
#     # Generate a clean, readable string of the mismatches
#     example_string <- paste(capture.output(print(examples)), collapse = "\n")
#     
#     warning(paste0(
#       "Found ", nrow(unmatched_reviews), " row(s) in 'review' that do not exist in 'deploy'!\n",
#       "Showing the first few unmatched pairs:\n",
#       example_string
#     ))
#   }
#   
#   # 2. Merge dataframes based on user preference
#   if (remove_unmatched_deploy) {
#     # Inner join drops any 'deploy' rows that don't match 'review'
#     merged_df <- inner_join(deploy_df, review_df, by = c("cruise", "dat_number"))
#   } else {
#     # Left join keeps all 'deploy' rows, filling missing 'review' data with NA
#     merged_df <- left_join(deploy_df, review_df, by = c("cruise", "dat_number"))
#   }
#   
#   return(merged_df)
# }


################################
# Merge Raven Selection Tables #
################################
#merge all selection tables in myPath and use clean_names to format column names
merge_raven_tables <- function(folder_path, time_zone = "UTC") {
  
  file_list <- list.files(path = folder_path, pattern = "\\.selections\\.txt$", full.names = TRUE)
  
  if (length(file_list) == 0) {
    stop("No selection table files ending in '.selections.txt' were found.")
  }
  
  process_single_file <- function(file_path) {
    fname <- basename(file_path)
    name_parts <- str_split(fname, "_")[[1]]
    
    cruise_num <- name_parts[1]
    dat_num    <- name_parts[2]
    date_str   <- name_parts[3]
    time_str   <- str_extract(name_parts[4], "^\\d{4}")
    
    raw_datetime <- paste(date_str, time_str)
    
    table_data <- read_delim(file_path, delim = "\t", show_col_types = FALSE)
    
    if (nrow(table_data) == 0) return(NULL)
    
    # Clean the names of THIS file FIRST before adding metadata
    table_data <- table_data %>% clean_names()
    
    # Add metadata
    table_data <- table_data %>%
      mutate(
        filename = fname,
        cruise_number = cruise_num,
        dat_number = dat_num,
        date = as.POSIXct(raw_datetime, format = "%Y-%m-%dT%H:%M:%SZ",
                          tz = "UTC")
      )
    prep_deploy_data <- function(sightings_path, deploy_df, species_ids,
                             output_path = NULL) {
  
  # 1. Load sightings from path; deploy is already a dataframe
  sightings_raw <- read_csv(here(sightings_path), show_col_types = FALSE) %>%
    clean_names()
  
  # 2. Clean and format deploy
  deploy_clean <- deploy_df %>%
    mutate(
      cruise          = as.character(cruise),
      sonobuoy_number = as.character(sonobuoy_number),
      dat_number      = as.character(dat_tape_number),
      # tz = "UTC" is intentional: the raw strings end in "Z" (UTC/Zulu).
      # Parsing with tz = "UTC" stores the literal clock digits with zero
      # offset applied, so the result is identical no matter what machine
      # or locale this runs on. Do NOT change this to tz = "" or Sys.timezone().
      date            = as.POSIXct(date, format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC")
    )
  
  # 3. Clean and format sightings
  sightings_clean <- sightings_raw %>%
    mutate(
      cruise           = as.character(cruise),
      sonobuoy_number  = as.character(sonobuoy_number),
      # ASSUMPTION: sightings has a column that becomes `dat_number` after
      # clean_names(). If it's actually named `dat_tape_number` (like deploy's
      # raw column), change the right-hand side below to
      # as.character(dat_tape_number). Verify against your actual column names.
      dat_number       = as.character(dat_number),
      sighting_species = as.character(sighting_species),
      near_species     = as.character(near_species)
    )
  
  # 3.5 HARD CHECK: every deploy row must have a matching sightings row,
  #     matched on cruise + sonobuoy_number + dat_number. It's fine for
  #     sightings to have combos that aren't in deploy — only the reverse
  #     direction is checked. Any mismatch halts execution.
  deploy_keys <- deploy_clean %>%
    select(cruise, sonobuoy_number, dat_number) %>%
    distinct()
  
  sightings_keys <- sightings_clean %>%
    select(cruise, sonobuoy_number, dat_number) %>%
    distinct()
  
  missing_in_sightings <- anti_join(
    deploy_keys, sightings_keys,
    by = c("cruise", "sonobuoy_number", "dat_number")
  )
  
  if (nrow(missing_in_sightings) > 0) {
    missing_list <- missing_in_sightings %>%
      mutate(
        combo = paste0(
          "cruise=", cruise,
          ", sonobuoy_number=", sonobuoy_number,
          ", dat_number=", dat_number
        )
      ) %>%
      pull(combo)
    
    stop(
      "prep_deploy_data(): ", nrow(missing_in_sightings),
      " deploy row(s) have no matching entry in sightings ",
      "(matched on cruise + sonobuoy_number + dat_number):\n",
      paste0("  - ", missing_list, collapse = "\n"),
      call. = FALSE
    )
  }
  
  # 4. Get all cruise/sonobuoy combos present in sightings (for NA flagging)
  #    NOTE: this stays on 2 keys (cruise/sonobuoy_number) for the species
  #    loop below, which is about linking a *species observation* to a
  #    deployment record, not the same thing as the strict existence check
  #    above. Given the check in step 3.5 now guarantees every deploy combo
  #    has a matching sightings combo, the is.na(.in_sightings) branch below
  #    is effectively a redundant safety net rather than a live code path —
  #    left in place intentionally.
  sightings_combos <- sightings_clean %>%
    select(cruise, sonobuoy_number) %>%
    distinct() %>%
    mutate(.in_sightings = TRUE)
  
  # 5. Loop over species_ids with purrr::reduce, adding one column per species
  processed_deploy <- purrr::reduce(
    as.character(species_ids),
    .init = deploy_clean,
    function(current_df, sp_id) {
      
      new_col_name <- paste0(sp_id, "_sightings")
      
      species_matches <- sightings_clean %>%
        filter(sighting_species == sp_id | near_species == sp_id) %>%
        select(cruise, sonobuoy_number) %>%
        distinct() %>%
        mutate(!!new_col_name := TRUE)
      
      current_df %>%
        left_join(species_matches, by = c("cruise", "sonobuoy_number")) %>%
        left_join(sightings_combos, by = c("cruise", "sonobuoy_number")) %>%
        mutate(
          !!new_col_name := case_when(
            is.na(.in_sightings)          ~ NA,
            .data[[new_col_name]] == TRUE ~ TRUE,
            TRUE                          ~ FALSE
          )
        ) %>%
        select(-.in_sightings)
    }
  )
  
  # 6. Optionally save to .rds
  if (!is.null(output_path)) {
    saveRDS(processed_deploy, file = here(output_path))
    message("Output saved to: ", here(output_path))
  }
  
  return(processed_deploy)
}
    
    return(table_data)
  }
  
  # map_df combines them cleanly, filling missing columns with NA
  merged_df <- map_df(file_list, process_single_file)
  
  return(merged_df)
}

#######################################################
# Validate Raven Selection Tables for Effort Analysis #
#######################################################
validate_effort_data <- function(folder_path) {
  
  # Get all .txt files in the folder
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) {
    message("No .txt files found in the specified folder.")
    return(NULL)
  }
  
  message(paste("Found", length(txt_files), ".txt files. Starting validation...\n"))
  
  # Define expected filename pattern
  filename_pattern <- "^(\\d+)_([^_]+)_(\\d{6})_(\\d{4})\\.Table\\.1\\.selections\\.txt$"
  
  # Initialize error tracking
  errors_list <- list()
  file_summary <- list()
  
  # Check 1: Validate all filenames
  basenames <- basename(txt_files)
  invalid_files <- basenames[!str_detect(basenames, filename_pattern)]
  
  if (length(invalid_files) > 0) {
    errors_list$invalid_filenames <- tibble(
      error_type = "Invalid Filename Format",
      filename = invalid_files,
      details = "Does not match expected pattern: Cruise#_RecordingName_YYMMDD_HHmm.Table.1.selections.txt"
    )
  }
  
  # Process each file for detailed validation
  for (file_path in txt_files) {
    
    filename <- basename(file_path)
    file_errors <- list()
    
    # Skip files with invalid names (already captured above)
    if (filename %in% invalid_files) {
      next
    }
    
    # Try to read the file
    df <- tryCatch({
      read_delim(file_path, delim = "\t", show_col_types = FALSE) %>%
        clean_names()
    }, error = function(e) {
      file_errors$read_error <- tibble(
        error_type = "File Read Error",
        filename = filename,
        details = as.character(e$message)
      )
      return(NULL)
    })
    
    if (is.null(df)) {
      errors_list[[filename]] <- bind_rows(file_errors)
      next
    }
    
    # Check 2: Validate required columns exist
    required_cols <- c("begin_time_s", "end_time_s", "start_end", "detection", "quality")
    missing_cols <- setdiff(required_cols, names(df))
    
    if (length(missing_cols) > 0) {
      file_errors$missing_columns <- tibble(
        error_type = "Missing Columns",
        filename = filename,
        details = paste("Missing:", paste(missing_cols, collapse = ", "))
      )
    }
    
    # If required columns are missing, can't continue validation for this file
    if (length(missing_cols) > 0) {
      errors_list[[filename]] <- bind_rows(file_errors)
      next
    }
    
    # Check 3: Identify all effort/noise annotations
    # All (1, 9, 9) annotations - first is start_effort, rest are restart_noise
    start_annotations <- df %>% 
      filter(start_end == 1, detection == 9, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(annotation_order = row_number())
    
    # All (3, 9, 9) annotations - last is end_effort
    end_effort_candidates <- df %>% 
      filter(start_end == 3, detection == 9, quality == 9) %>%
      arrange(begin_time_s)
    
    # All (3, 1, 9) annotations - these are end_noise
    end_noise <- df %>% 
      filter(start_end == 3, detection == 1, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(noise_bout = row_number())
    
    # Check 4: Validate exactly one start_effort (first (1,9,9))
    if (nrow(start_annotations) < 1) {
      file_errors$no_start_effort <- tibble(
        error_type = "Missing Start Effort",
        filename = filename,
        details = "No (1, 9, 9) annotations found - missing start_effort"
      )
    }
    
    # Check 5: Validate exactly one end_effort (should be exactly one (3,9,9))
    if (nrow(end_effort_candidates) != 1) {
      file_errors$end_effort <- tibble(
        error_type = "End Effort Error",
        filename = filename,
        details = paste("Expected exactly 1 end_effort (3, 9, 9), found:", nrow(end_effort_candidates))
      )
    }
    
    # Check 6: Validate noise bout structure
    # Number of restart_noise should equal number of end_noise
    n_restart_noise <- nrow(start_annotations) - 1  # All (1,9,9) except the first
    n_end_noise <- nrow(end_noise)
    
    if (n_restart_noise != n_end_noise) {
      file_errors$unpaired_noise <- tibble(
        error_type = "Unpaired Noise Annotations",
        filename = filename,
        details = paste("restart_noise count:", n_restart_noise, 
                        "| end_noise count:", n_end_noise,
                        "- Each end_noise (3,1,9) should be followed by restart_noise (1,9,9)")
      )
    }
    
    # Check 7: Validate noise bout sequence and calculate durations
    if (nrow(start_annotations) > 1 && nrow(end_noise) > 0) {
      
      # Get restart_noise annotations (all (1,9,9) except first)
      restart_noise <- start_annotations %>%
        filter(annotation_order > 1) %>%
        mutate(noise_bout = row_number())
      
      # Only proceed if counts match
      if (nrow(restart_noise) == nrow(end_noise)) {
        
        # Pair up end_noise with restart_noise
        noise_bouts <- end_noise %>%
          select(noise_bout, end_noise_time = begin_time_s) %>%
          left_join(
            restart_noise %>% select(noise_bout, restart_noise_time = begin_time_s),
            by = "noise_bout"
          ) %>%
          mutate(duration = restart_noise_time - end_noise_time)
        
        # Check that restart_noise comes after end_noise
        invalid_order <- noise_bouts %>%
          filter(restart_noise_time <= end_noise_time)
        
        if (nrow(invalid_order) > 0) {
          file_errors$noise_order <- tibble(
            error_type = "Invalid Noise Bout Order",
            filename = filename,
            details = paste("Noise bout(s)", paste(invalid_order$noise_bout, collapse = ", "),
                            "have restart_noise at or before end_noise")
          )
        }
        
        # Check for negative durations
        negative_duration <- noise_bouts %>%
          filter(duration < 0)
        
        if (nrow(negative_duration) > 0) {
          file_errors$negative_duration <- tibble(
            error_type = "Negative Noise Duration",
            filename = filename,
            details = paste("Noise bout(s)", paste(negative_duration$noise_bout, collapse = ", "),
                            "have negative duration")
          )
        }
        
        # Check for overlapping noise bouts
        if (nrow(noise_bouts) > 1) {
          overlaps <- c()
          for (i in 2:nrow(noise_bouts)) {
            # Current end_noise should come after previous restart_noise
            if (noise_bouts$end_noise_time[i] < noise_bouts$restart_noise_time[i-1]) {
              overlaps <- c(overlaps, paste("Bout", i-1, "and Bout", i))
            }
          }
          
          if (length(overlaps) > 0) {
            file_errors$noise_overlap <- tibble(
              error_type = "Overlapping Noise Bouts",
              filename = filename,
              details = paste("Overlaps found between:", paste(overlaps, collapse = "; "))
            )
          }
        }
      }
    }
    
    # Check 8: Validate temporal ordering of all annotations
    if (nrow(start_annotations) >= 1 && nrow(end_effort_candidates) == 1) {
      start_effort_time <- start_annotations$begin_time_s[1]
      end_effort_time <- end_effort_candidates$begin_time_s[1]
      
      if (end_effort_time <= start_effort_time) {
        file_errors$effort_order <- tibble(
          error_type = "Invalid Effort Order",
          filename = filename,
          details = "end_effort comes at or before start_effort"
        )
      }
    }
    
    # Store file summary (even if no errors)
    file_summary[[filename]] <- tibble(
      filename = filename,
      total_rows = nrow(df),
      start_effort_count = ifelse(nrow(start_annotations) >= 1, 1, 0),
      restart_noise_count = max(0, nrow(start_annotations) - 1),
      end_noise_count = nrow(end_noise),
      end_effort_count = nrow(end_effort_candidates),
      has_errors = length(file_errors) > 0
    )
    
    # Store errors for this file if any exist
    if (length(file_errors) > 0) {
      errors_list[[filename]] <- bind_rows(file_errors)
    }
  }
  
  # Compile results
  all_errors <- bind_rows(errors_list)
  all_summaries <- bind_rows(file_summary)
  
  # Print summary
  cat("\n==================== VALIDATION SUMMARY ====================\n")
  cat(paste("Total files checked:", length(txt_files), "\n"))
  cat(paste("Files with errors:", sum(all_summaries$has_errors, na.rm = TRUE), "\n"))
  cat(paste("Files passing all checks:", sum(!all_summaries$has_errors, na.rm = TRUE), "\n"))
  
  if (nrow(all_errors) > 0) {
    cat("\n==================== ERRORS BY TYPE ====================\n")
    error_summary <- all_errors %>%
      group_by(error_type) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(desc(count))
    
    print(error_summary)
    
    cat("\n==================== DETAILED ERRORS ====================\n")
    print(all_errors, n = Inf)
  } else {
    cat("\n✓ All files passed validation!\n")
  }
  
  cat("\n==================== FILE SUMMARY ====================\n")
  print(all_summaries, n = Inf)
  
  # Return results invisibly
  invisible(list(
    errors = all_errors,
    summary = all_summaries,
    has_errors = nrow(all_errors) > 0
  ))
}

# Example usage:
# validation_results <- validate_effort_data("path/to/your/folder")
# 
# # To save results for further review:
# if (validation_results$has_errors) {
#   write_csv(validation_results$errors, "validation_errors.csv")
#   write_csv(validation_results$summary, "validation_summary.csv")
# }


######################################################
# Process Raven Selection Tables for Effort Analysis #
######################################################

#' @param folder_path Path to folder containing Raven selection table text files
#' @return A dataframe with effort metrics for each recording
#' @export
process_effort_analysis <- function(folder_path) {
  
  # Get all .txt files in the folder
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) {
    stop("No .txt files found in the specified folder.")
  }
  
  message(paste("Processing", length(txt_files), "files..."))
  
  # Define expected filename pattern
  filename_pattern <- "^(\\d+)_([^_]+)_(\\d{6})_(\\d{4})\\.Table\\.1\\.selections\\.txt$"
  
  # Initialize results list
  results_list <- list()
  
  # Process each file
  for (file_path in txt_files) {
    
    filename <- basename(file_path)
    
    # Parse filename components
    cruise_num <- str_match(filename, filename_pattern)[, 2]
    recording_name <- str_match(filename, filename_pattern)[, 3]
    date_str <- str_match(filename, filename_pattern)[, 4]
    time_str <- str_match(filename, filename_pattern)[, 5]
    
    # Parse datetime from filename (without timezone)
    # Handle years from 1990s and 2000s
    yy <- as.integer(str_sub(date_str, 1, 2))
    year <- ifelse(yy >= 90, 1900 + yy, 2000 + yy)
    month <- as.integer(str_sub(date_str, 3, 4))
    day <- as.integer(str_sub(date_str, 5, 6))
    hour <- as.integer(str_sub(time_str, 1, 2))
    minute <- as.integer(str_sub(time_str, 3, 4))
    
    recording_start <- as.POSIXct(
      paste0(year, "-", sprintf("%02d", month), "-", sprintf("%02d", day), " ",
             sprintf("%02d", hour), ":", sprintf("%02d", minute), ":00"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""  # No timezone
    )
    
    # Read and clean data
    df <- read_delim(file_path, delim = "\t", show_col_types = FALSE) %>%
      clean_names() 

    # Identify start_effort (first (1, 9, 9))
    start_effort <- df %>% 
      filter(start_end == 1, detection == 9, quality == 9) %>%
      arrange(begin_time_s) %>%
      slice(1)
    
    # Identify end_effort ((3, 9, 9))
    end_effort <- df %>% 
      filter(start_end == 3, detection == 9, quality == 9)
    
    # Calculate effort start and end datetimes
    effort_start_datetime <- recording_start + seconds(start_effort$begin_time_s)
    effort_end_datetime <- recording_start + seconds(end_effort$begin_time_s)
    
    # Identify all (1, 9, 9) annotations - first is start_effort, rest are restart_noise
    all_starts <- df %>% 
      filter(start_end == 1, detection == 9, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(annotation_order = row_number())
    
    # Get restart_noise (all (1, 9, 9) except the first)
    restart_noise <- all_starts %>%
      filter(annotation_order > 1) %>%
      mutate(noise_bout = row_number())
    
    # Identify end_noise ((3, 1, 9))
    end_noise <- df %>% 
      filter(start_end == 3, detection == 1, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(noise_bout = row_number())
    
    # Calculate noise duration
    if (nrow(end_noise) > 0 && nrow(restart_noise) > 0) {
      noise_bouts <- end_noise %>%
        select(noise_bout, end_noise_time = begin_time_s) %>%
        left_join(
          restart_noise %>% select(noise_bout, restart_noise_time = begin_time_s),
          by = "noise_bout"
        ) %>%
        mutate(duration = restart_noise_time - end_noise_time)
      
      sum_noise_s <- sum(noise_bouts$duration)
    } else {
      sum_noise_s <- 0
    }
    
    # Calculate effort in minutes
    total_effort_s <- end_effort$begin_time_s - start_effort$begin_time_s
    good_effort_s <- total_effort_s - sum_noise_s
    effort_min <- good_effort_s / 60
    
    # Store results
    results_list[[filename]] <- tibble(
      filename = filename,
      cruise = cruise_num,
      recording_name = recording_name,
      start_datetime = effort_start_datetime,
      end_datetime = effort_end_datetime,
      sum_noise_s = sum_noise_s,
      effort_min = effort_min
    )
  }
  
  # Combine all results
  results_df <- bind_rows(results_list)
  
  # Save as RDS file
  data_path <- here("data/")
  folder_name <- basename(normalizePath(folder_path))
  output_filename <- paste0(folder_name, "_effort.rds")
  output_path <- file.path(data_path, output_filename)
  
  saveRDS(results_df, output_path)
  
  message(paste("Processed", nrow(results_df), "files successfully."))
  message(paste("Results saved to:", data_path))
  
  return(results_df)
}

# Example usage:
# results <- process_effort_analysis("path/to/your/folder")

##################################
# Merge and Validate Deployments #
##################################
#Append existing be_deploy, be_effort, and e_review tables (and look for obvious errors)
merge_and_validate_deployments <- function(deploy_df, effort_df, be_review_df) {
  library(dplyr)
  
  # 1. Force ID columns to numeric to prevent data type mismatch errors across ALL dataframes
  deploy_df <- deploy_df %>%
    dplyr::mutate(
      cruise = as.numeric(cruise),
      sonobuoy_number = as.numeric(sonobuoy_number)
    )
  
  effort_df <- effort_df %>%
    dplyr::mutate(
      cruise = as.numeric(cruise),
      sonobuoy_number = as.numeric(sonobuoy_number)
    )
  
  be_review_df <- be_review_df %>%
    dplyr::mutate(
      cruise = as.numeric(cruise),
      sonobuoy_number = as.numeric(sonobuoy_number)
    )
  
  # 2. Perform the left joins sequentially
  combined_df <- deploy_df %>%
    dplyr::left_join(effort_df, by = c("cruise", "sonobuoy_number")) %>%
    dplyr::left_join(be_review_df, by = c("cruise", "sonobuoy_number"))
  
  cat("\n--- RUNNING DIAGNOSTIC CHECKS ---\n")
  
  # 3. Check 1: deploy rows with NO matching effort
  missing_effort <- combined_df %>%
    dplyr::filter(is.na(recording_name)) %>%
    dplyr::select(cruise, sonobuoy_number) %>%
    dplyr::distinct()
  
  if (nrow(missing_effort) > 0) {
    warning("The following deployment rows do not have an associated effort value:")
    print(missing_effort)
  } else {
    message("✓ Success: All deployment rows have associated effort data.")
  }
  
  # 4. Check 2: deploy rows with MULTIPLE matching effort rows
  duplicate_effort_checks <- effort_df %>%
    dplyr::group_by(cruise, sonobuoy_number) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_effort_checks) > 0) {
    warning("The following cruise/sonobuoy combinations have multiple rows in effort (causes duplicate rows):")
    print(duplicate_effort_checks)
  } else {
    message("✓ Success: No duplicate effort matches found.")
  }
  
  # 5. Check 3: New Check for be_review mismatches
  # (Assumes 'be_review_df' has a unique column name, replace 'status' below if needed)
  # If you don't know a unique column, we check if the entire joined block from be_review is missing
  missing_be_review <- combined_df %>%
    dplyr::filter(is.na(recording_quality_code)) %>% # <-- Change 'status' to a column unique to be_review (e.g., 'reviewer', 'comments')
    dplyr::select(cruise, sonobuoy_number) %>%
    dplyr::distinct()
  
  if (nrow(missing_be_review) > 0) {
    warning("The following deployment rows do not have an associated be_review value:")
    print(missing_be_review)
  } else {
    message("✓ Success: All deployment rows have associated be_review data.")
  }
  
  # 6. Check 4: deploy rows with MULTIPLE matching be_review rows
  duplicate_be_checks <- be_review_df %>%
    dplyr::group_by(cruise, sonobuoy_number) %>%
    dplyr::tally() %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_be_checks) > 0) {
    warning("The following cruise/sonobuoy combinations have multiple rows in be_review (causes duplicate rows):")
    print(duplicate_be_checks)
  } else {
    message("✓ Success: No duplicate be_review matches found.")
  }
  
  cat("---------------------------------\n\n")
  
  # Return the fully merged dataframe
  return(combined_df)
}


########################
# Acoustic Violin Plot #
########################
create_acoustic_violin_plots <- function(data, hjust = -0.15, vjust = -0.5) {
  
  # 1. Clean, rename, and structure the long-format data variables
  plot_data <- data %>%
    dplyr::select(low_freq_hz, high_freq_hz, dur_90_percent_s) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "metric",
      values_to = "value"
    ) %>%
    dplyr::mutate(metric = dplyr::recode_factor(
      metric,
      "low_freq_hz"       = "Low Frequency (Hz)",
      "high_freq_hz"      = "High Frequency (Hz)",
      "dur_90_percent_s"  = "Duration (s)"
    ))
  
  # 2. Compute the exact medians dynamically per metric for geom_text
  median_labels <- plot_data %>%
    dplyr::group_by(metric) %>%
    dplyr::summarise(
      median_val = median(value, na.rm = TRUE),
      # FIX: Extracting the scalar character explicitly forces a size of 1
      label_text = case_when(
        as.character(metric[1]) == "Duration (s)" ~ format(round(median_val, 2), nsmall = 2), 
        TRUE                                      ~ format(round(median_val, 0), nsmall = 0)
      )
    )
  
  # 3. Split dataset and labels for Frequencies vs. Duration
  freq_data   <- plot_data %>% dplyr::filter(metric %in% c("Low Frequency (Hz)", "High Frequency (Hz)"))
  freq_labels <- median_labels %>% dplyr::filter(metric %in% c("Low Frequency (Hz)", "High Frequency (Hz)"))
  
  dur_data <- plot_data %>% dplyr::filter(metric == "Duration (s)")
  dur_density <- stats::density(dur_data$value[is.finite(dur_data$value)])
  peak_indices <- which(diff(sign(diff(dur_density$y))) == -2) + 1
  dur_peaks <- data.frame(
    peak_val = dur_density$x[peak_indices],
    peak_density = dur_density$y[peak_indices]
  ) %>%
    dplyr::slice_max(peak_density, n = 2, with_ties = FALSE) %>%
    dplyr::arrange(peak_val) %>%
    dplyr::mutate(
      metric = factor("Duration (s)", levels = levels(plot_data$metric)),
      label_text = paste0(format(round(peak_val, 2), nsmall = 2), " s")
    )
  
  # 4. Shared theme settings
  shared_theme <- theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(family = "serif", face = "bold", size = 11)
    )
  
  # 5. Build Frequency Component
  p_freq <- ggplot(freq_data, aes(x = metric, y = value, fill = metric)) +
    geom_violin(alpha = 0.7, color = "black", linewidth = 0.5, quantiles = 0.5, quantile.linetype = "solid") +
    geom_text(
      data = freq_labels,
      aes(x = metric, y = median_val, label = label_text),
      inherit.aes = FALSE, 
      family = "serif",
      fontface = "bold",
      size = 3.5,
      hjust = hjust,    
      vjust = vjust     
    ) +
    scale_fill_manual(values = c("#B3CDE3", "#CCEBC5")) + 
    facet_wrap(~ metric, scales = "fixed") + 
    labs(y = "Frequency (Hz)") +
    shared_theme
  
  # 6. Build Duration Component
  p_dur <- ggplot(dur_data, aes(x = value, y = metric, fill = metric)) +
    geom_violin(alpha = 0.7, color = "black", linewidth = 0.5, orientation = "y") +
    geom_vline(
      data = dur_peaks,
      aes(xintercept = peak_val),
      inherit.aes = FALSE,
      linewidth = 0.5
    ) +
    geom_text(
      data = dur_peaks,
      aes(x = peak_val, y = metric, label = label_text),
      inherit.aes = FALSE,
      family = "serif",
      fontface = "bold",
      size = 3.5,
      vjust = -1
    ) +
    scale_fill_manual(values = c("#DECBE4")) + 
    facet_wrap(~ metric) + 
    labs(x = "Duration (seconds)", y = NULL) +
    shared_theme +
    theme(
      axis.title.x = element_text(),
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  
  # 7. Layout composition
  combined_plot <- (p_freq / p_dur) +
    plot_layout(heights = c(2, 1))
    # plot_annotation(title = "Distribution of Acoustic Features")
  
  return(combined_plot)
}

######################
# Plot IPI Histogram #
######################


plot_ipi_histogram <- function(ipi_data, num_peaks = NULL) {
  
  # 1. Strip down to a completely clean, raw numeric vector
  clean_ipi <- as.numeric(na.omit(ipi_data))
  clean_ipi <- clean_ipi[is.finite(clean_ipi)]
  
  if(length(clean_ipi) == 0) stop("No valid numeric data found in ipi_data.")
  df <- data.frame(IPI = clean_ipi)
  
  # 2. Find peaks using R's built-in density function
  dens <- density(df$IPI)
  
  # Find local maxima (where density stops rising and starts falling)
  shapes <- diff(sign(diff(dens$y)))
  peak_indices <- which(shapes == -2) + 1
  
  # Extract the X-coordinates (IPI values) of the peaks
  detected_peaks <- dens$x[peak_indices]
  peak_densities <- dens$y[peak_indices]
  
  # Order peaks by how tall they are (most dominant peaks first)
  peak_table <- data.frame(peak = detected_peaks, density = peak_densities) %>%
    arrange(desc(density))
  
  # 3. Determine how many peaks to use
  if (!is.null(num_peaks)) {
    n <- min(num_peaks, nrow(peak_table))
  } else {
    # Automatically pick peaks that are at least 10% of the tallest peak's height
    n <- sum(peak_table$density >= (max(peak_table$density) * 0.10))
    n <- max(1, min(n, 3)) # Cap it between 1 and 3 peaks automatically
  }
  
  final_peaks <- head(peak_table$peak, n)
  
  # 4. Calculate medians for data surrounding each peak
  if (length(final_peaks) == 1) {
    # Unimodal: Just use global median
    peak_medians <- data.frame(Peak_ID = "Peak 1", Median_Val = median(df$IPI))
  } else {
    # Multimodal: Assign points to the closest peak to find peak-specific medians
    final_peaks <- sort(final_peaks)
    peak_medians <- df %>%
      rowwise() %>%
      mutate(Closest_Peak = which.min(abs(IPI - final_peaks))) %>%
      ungroup() %>%
      group_by(Closest_Peak) %>%
      summarise(Median_Val = median(IPI), .groups = 'drop') %>%
      mutate(Peak_ID = paste("Peak", seq_len(n())))
  }
  
  # 5. Build the publication-ready ggplot
  p <- ggplot(df, aes(x = IPI)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "white", color = "black", linewidth = 0.5) +
    geom_density(color = "dimgray", linewidth = 0.8, linetype = "solid") +
    geom_vline(data = peak_medians, aes(xintercept = Median_Val), 
               color = "firebrick", linetype = "dashed", linewidth = 0.8) +
    geom_text(data = peak_medians, 
              aes(x = Median_Val, y = Inf, 
                  label = paste0("Median: ", round(Median_Val, 3), "s")), 
              vjust = 2, hjust = -0.1, color = "firebrick", fontface = "bold", size = 3.5) +
    theme_classic(base_size = 14) +
    theme(
      axis.line = element_line(color = "black", linewidth = 0.8),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text = element_text(color = "black")
    ) +
    labs(
      title = "Distribution of Inter-Pulse Intervals (IPI)",
      x = "Inter-Pulse Interval (seconds)",
      y = "Density"
    )
  
  print(p)
  return(peak_medians)
}


# #######################
# # add offsetGMT value #
# #######################
# # =============================================================================
# # FUNCTION 1: Process .das files and add offsetGMT column to brydes_df
# # =============================================================================
# 
# add_offset_gmt <- function(df, das_dir = here("data/surveyData")) {
#   
#   # --- Step 1: Find all .das files and process/cache as .rds ---
#   das_files <- list.files(das_dir, pattern = "\\.das$", 
#                           full.names = TRUE, ignore.case = TRUE)
#   
#   if (length(das_files) == 0) {
#     warning("No .das files found in: ", das_dir)
#     return(df %>% mutate(offsetGMT = NA_character_))
#   }
#   
#   message("Processing ", length(das_files), " .das file(s)...")
#   
#   das_cache <- map(das_files, function(das_path) {
#     
#     rds_path <- sub("\\.das$", ".rds", das_path, ignore.case = TRUE)
#     
#     # Load cached RDS if it exists, otherwise process and save
#     if (file.exists(rds_path)) {
#       message("  Loading cached RDS: ", basename(rds_path))
#       processed <- readRDS(rds_path)
#     } else {
#       message("  Processing: ", basename(das_path))
#       raw       <- das_read(das_path)
#       processed <- das_process(raw)
#       saveRDS(processed, rds_path)
#       message("  Saved RDS: ", basename(rds_path))
#     }
#     
#     list(filename = basename(das_path), data = processed)
#   })
#   
#   # --- Step 2: For each row in df, find matching RDS and determine offsetGMT ---
#   
#   get_offset_for_row <- function(cruise_val, date_val) {
#     
#     # Find the cached dataset whose filename contains the cruise number
#     match_idx <- detect_index(das_cache, function(x) {
#       grepl(as.character(cruise_val), x$filename, fixed = TRUE)
#     })
#     
#     if (match_idx == 0) {
#       warning("No .das file found containing cruise: ", cruise_val)
#       return(NA_character_)
#     }
#     
#     das_data <- das_cache[[match_idx]]$data
#     
#     # Ensure DateTime and OffsetGMT columns exist
#     if (!all(c("DateTime", "OffsetGMT") %in% names(das_data))) {
#       warning("Required columns (DateTime, OffsetGMT) not found for cruise: ", 
#               cruise_val)
#       return(NA_character_)
#     }
#     
#     # # Define ±24 hr window around the row's date
#     # window_start <- date_val - dhours(24)
#     # window_end   <- date_val + dhours(24)
#     # Define ±6 hr window around the row's date
#     window_start <- date_val - dhours(1)
#     window_end   <- date_val + dhours(1)
#     
#     window_data <- das_data %>%
#       filter(!is.na(OffsetGMT),
#              DateTime >= window_start,
#              DateTime <= window_end)
#     
#     if (nrow(window_data) == 0) {
#       warning("No DAS records within ±24 hrs for cruise ", cruise_val, 
#               " around ", date_val)
#       return(NA_character_)
#     }
#     
#     unique_offsets <- unique(window_data$OffsetGMT)
#     
#     if (length(unique_offsets) == 1) {
#       # Stable offset — return as character (can be coerced later)
#       return(as.character(unique_offsets))
#     } else {
#       return("CHANGE")
#     }
#   }
#   
#   # Apply row-wise
#   message("Determining offsetGMT for ", nrow(df), " row(s)...")
#   
#   df <- df %>%
#     mutate(
#       offsetGMT = map2_chr(
#         cruise, date,
#         ~ get_offset_for_row(cruise_val = .x, date_val = .y)
#       )
#     )
#   
#   n_change <- sum(df$offsetGMT == "CHANGE", na.rm = TRUE)
#   n_na     <- sum(is.na(df$offsetGMT))
#   n_ok     <- nrow(df) - n_change - n_na
#   
#   message(
#     "offsetGMT summary:\n",
#     "  Stable offsets: ", n_ok, "\n",
#     "  CHANGE flags:   ", n_change, "\n",
#     "  NAs (no match): ", n_na
#   )
#   
#   df
# }
# 
# 
# # =============================================================================
# # FUNCTION 2: Convert 'date' to 'date_UTC' using 'offsetGMT'
# # =============================================================================
# 
# add_date_utc <- function(df) {
#   
#   if (!all(c("date", "offsetGMT") %in% names(df))) {
#     stop("df must contain 'date' and 'offsetGMT' columns. ",
#          "Run add_offset_gmt() first.")
#   }
#   
#   df <- df %>%
#     mutate(
#       date_UTC = case_when(
#         is.na(offsetGMT)          ~ NA_POSIXct_,
#         offsetGMT == "CHANGE"     ~ NA_POSIXct_,
#         TRUE ~ {
#           offset_hours <- as.numeric(offsetGMT)
#           # Local time = UTC + offset, so UTC = local - offset
#           date - dhours(offset_hours)
#         }
#       )
#     )
#   
#   n_converted <- sum(!is.na(df$date_UTC))
#   n_na        <- sum(is.na(df$date_UTC))
#   
#   message(
#     "date_UTC summary:\n",
#     "  Successfully converted: ", n_converted, "\n",
#     "  Left as NA:             ", n_na, 
#     " (CHANGE flags or missing offsetGMT)"
#   )
#   
#   df
# }
# 
# 
# # =============================================================================
# # USAGE
# # =============================================================================
# 
# # Step 1 — adds offsetGMT; review CHANGE rows before proceeding
# #brydes_df <- brydes_df %>% add_offset_gmt()
# 
# # Optional: inspect rows needing manual review
# #brydes_df %>% filter(offsetGMT == "CHANGE" | is.na(offsetGMT)) %>% View()
# 
# # Step 2 — convert to UTC once you're satisfied with offsetGMT values
# #brydes_df <- brydes_df %>% add_date_utc()
