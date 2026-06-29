process_effort_analysis <- function(
    folder_path,
    output_file = NULL
) {
  
  # ── File discovery ────────────────────────────────────────────────────────
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) {
    stop("No .txt files found in: ", folder_path)
  }
  
  message("Processing ", length(txt_files), " file(s)...")
  
  filename_pattern <- "^(\\d+)_([^_]+)_(\\d{6})_(\\d{4})\\.Table\\.1\\.selections\\.txt$"
  
  # ── Per-file processing ───────────────────────────────────────────────────
  process_file <- function(file_path) {
    
    filename <- basename(file_path)
    parts    <- str_match(filename, filename_pattern)
    
    cruise_num     <- parts[, 2]
    recording_name <- parts[, 3]
    date_str       <- parts[, 4]
    time_str       <- parts[, 5]
    
    # Parse datetime from filename components (no timezone)
    yy    <- as.integer(str_sub(date_str, 1, 2))
    year  <- ifelse(yy >= 90, 1900 + yy, 2000 + yy)
    month <- as.integer(str_sub(date_str, 3, 4))
    day   <- as.integer(str_sub(date_str, 5, 6))
    hour  <- as.integer(str_sub(time_str, 1, 2))
    min   <- as.integer(str_sub(time_str, 3, 4))
    
    recording_start <- as.POSIXct(
      sprintf("%04d-%02d-%02d %02d:%02d:00", year, month, day, hour, min),
      format = "%Y-%m-%d %H:%M:%S",
      tz     = ""
    )
    
    # Read and clean file
    df <- read_delim(file_path, delim = "\t", show_col_types = FALSE) %>%
      clean_names()
    
    # ── Annotation extraction ──────────────────────────────────────────────
    all_starts <- df %>%
      filter(start_end == 1, detection == 9, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(annotation_order = row_number())
    
    start_effort  <- slice(all_starts, 1)
    restart_noise <- all_starts %>%
      filter(annotation_order > 1) %>%
      mutate(noise_bout = row_number())
    
    end_effort <- df %>%
      filter(start_end == 3, detection == 9, quality == 9)
    
    end_noise <- df %>%
      filter(start_end == 3, detection == 1, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(noise_bout = row_number())
    
    # ── Noise duration ─────────────────────────────────────────────────────
    sum_noise_s <- if (nrow(end_noise) > 0 && nrow(restart_noise) > 0) {
      end_noise %>%
        select(noise_bout, end_noise_time = begin_time_s) %>%
        left_join(
          restart_noise %>% select(noise_bout, restart_noise_time = begin_time_s),
          by = "noise_bout"
        ) %>%
        mutate(duration = restart_noise_time - end_noise_time) %>%
        pull(duration) %>%
        sum()
    } else {
      0
    }
    
    # ── Effort calculation ─────────────────────────────────────────────────
    total_effort_s <- end_effort$begin_time_s - start_effort$begin_time_s
    effort_min     <- (total_effort_s - sum_noise_s) / 60
    
    tibble(
      filename             = filename,
      cruise               = cruise_num,
      recording_name       = recording_name,
      dat_number           = as.numeric(str_remove(recording_name, ".*SBDAT")),
      start_datetime       = recording_start + seconds(start_effort$begin_time_s),
      end_datetime         = recording_start + seconds(end_effort$begin_time_s),
      sum_noise_s          = sum_noise_s,
      effort_min           = effort_min
    )
  }
  
  results_df <- map_df(txt_files, process_file)
  
  message("Processed ", nrow(results_df), " file(s) successfully.")
  
  # ── Optional save ─────────────────────────────────────────────────────────
  if (!is.null(output_file)) {
    saveRDS(results_df, file = output_file)
    message("Results saved to: ", output_file)
  }
  
  return(results_df)
}
