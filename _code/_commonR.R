# Load Libraries
library(here)
library(tidyverse)
library(purrr)
library(lubridate)
library(janitor)
library(warbleR)
library(Rraven)
library(dplyr)
library(stringr)
library(readr)

#Add general functions

################################
# Merge Raven Selection Tables #
################################

merge_raven_tables <- function(folder_path, time_zone = "UTC") {
  
  # 1. Get a list of all selection table files in the directory
  file_list <- list.files(path = folder_path, pattern = "\\.selections\\.txt$", full.names = TRUE)
  
  # Check if any files were actually found
  if (length(file_list) == 0) {
    stop("No selection table files ending in '.selections.txt' were found in the specified directory.")
  }
  
  # 2. Inner helper function to process a single file
  process_single_file <- function(file_path) {
    fname <- basename(file_path)
    
    # Split filename by "_"
    name_parts <- str_split(fname, "_")[[1]]
    
    cruise_num <- name_parts[1]
    dat_num    <- name_parts[2]
    date_str   <- name_parts[3]
    time_str   <- name_parts[4]
    
    # Isolate just the 4-digit HHMM time
    time_str   <- str_extract(time_str, "^\\d{4}")
    
    raw_datetime <- paste(date_str, time_str)
    
    # Read the tab-delimited Raven file
    table_data <- read_delim(file_path, delim = "\t", show_col_types = FALSE)
    
    # Return NULL safely if the file is completely empty
    if (nrow(table_data) == 0) return(NULL)
    
    # Add metadata and convert to POSIXct
    table_data <- table_data %>%
      mutate(
        filename = fname,
        cruise_number = cruise_num,
        dat_number = dat_num,
        date_time = as.POSIXct(raw_datetime, format = "%y%m%d %H%M", tz = time_zone)
      )
    
    return(table_data)
  }
  
  # 3. Use map_df to loop through files and bind them together
  merged_df <- map_df(file_list, process_single_file)
  
  # 4. Clean all column names to snake_case using janitor
  merged_df <- merged_df %>% 
    clean_names()
  
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
  folder_name <- basename(normalizePath(folder_path))
  output_filename <- paste0(folder_name, "_effort.rds")
  output_path <- file.path(folder_path, output_filename)
  
  saveRDS(results_df, output_path)
  
  message(paste("Processed", nrow(results_df), "files successfully."))
  message(paste("Results saved to:", output_path))
  
  return(results_df)
}

# Example usage:
# results <- process_effort_analysis("path/to/your/folder")
# View(results)

