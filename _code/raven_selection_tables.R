validate_merge_raven <- function(
    folder_path,
    merge          = TRUE,
    merge_on_errors = TRUE,
    output_file    = NULL,
    time_zone      = ""
) {
  
  # ── File discovery ────────────────────────────────────────────────────────
  txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  if (length(txt_files) == 0) {
    message("No .txt files found in: ", folder_path)
    return(invisible(NULL))
  }
  
  message("Found ", length(txt_files), " .txt file(s). Starting validation...\n")
  
  filename_pattern <- "^(\\d+)_([^_]+)_(\\d{6})_(\\d{4})\\.Table\\.1\\.selections\\.txt$"
  required_cols    <- c("begin_time_s", "end_time_s", "start_end", "detection", "quality")
  
  errors_list  <- list()
  file_summary <- list()
  
  # ── Per-file validation ───────────────────────────────────────────────────
  for (file_path in txt_files) {
    
    filename    <- basename(file_path)
    file_errors <- list()
    
    # Check 1: filename format
    if (!str_detect(filename, filename_pattern)) {
      errors_list[[filename]] <- tibble(
        error_type = "Invalid Filename Format",
        filename   = filename,
        details    = "Does not match: Cruise#_RecordingName_YYMMDD_HHmm.Table.1.selections.txt"
      )
      file_summary[[filename]] <- tibble(
        filename             = filename,
        total_rows           = NA_integer_,
        start_effort_count   = NA_integer_,
        restart_noise_count  = NA_integer_,
        end_noise_count      = NA_integer_,
        end_effort_count     = NA_integer_,
        has_errors           = TRUE
      )
      next
    }
    
    # Check 2: file readability
    df <- tryCatch(
      read_delim(file_path, delim = "\t", show_col_types = FALSE) %>% clean_names(),
      error = function(e) {
        file_errors$read_error <<- tibble(
          error_type = "File Read Error",
          filename   = filename,
          details    = as.character(e$message)
        )
        NULL
      }
    )
    
    if (is.null(df)) {
      errors_list[[filename]]  <- bind_rows(file_errors)
      file_summary[[filename]] <- tibble(
        filename             = filename,
        total_rows           = NA_integer_,
        start_effort_count   = NA_integer_,
        restart_noise_count  = NA_integer_,
        end_noise_count      = NA_integer_,
        end_effort_count     = NA_integer_,
        has_errors           = TRUE
      )
      next
    }
    
    # Check 3: required columns
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      file_errors$missing_columns <- tibble(
        error_type = "Missing Columns",
        filename   = filename,
        details    = paste("Missing:", paste(missing_cols, collapse = ", "))
      )
      errors_list[[filename]]  <- bind_rows(file_errors)
      file_summary[[filename]] <- tibble(
        filename             = filename,
        total_rows           = nrow(df),
        start_effort_count   = NA_integer_,
        restart_noise_count  = NA_integer_,
        end_noise_count      = NA_integer_,
        end_effort_count     = NA_integer_,
        has_errors           = TRUE
      )
      next
    }
    
    # ── Annotation extraction ────────────────────────────────────────────
    start_annotations     <- df %>%
      filter(start_end == 1, detection == 9, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(annotation_order = row_number())
    
    end_effort_candidates <- df %>%
      filter(start_end == 3, detection == 9, quality == 9) %>%
      arrange(begin_time_s)
    
    end_noise             <- df %>%
      filter(start_end == 3, detection == 1, quality == 9) %>%
      arrange(begin_time_s) %>%
      mutate(noise_bout = row_number())
    
    n_restart_noise <- max(0, nrow(start_annotations) - 1)
    n_end_noise     <- nrow(end_noise)
    
    # Check 4: start_effort present
    if (nrow(start_annotations) < 1) {
      file_errors$no_start_effort <- tibble(
        error_type = "Missing Start Effort",
        filename   = filename,
        details    = "No (1, 9, 9) annotations found — missing start_effort"
      )
    }
    
    # Check 5: exactly one end_effort
    if (nrow(end_effort_candidates) != 1) {
      file_errors$end_effort <- tibble(
        error_type = "End Effort Error",
        filename   = filename,
        details    = paste("Expected exactly 1 end_effort (3,9,9), found:", nrow(end_effort_candidates))
      )
    }
    
    # Check 6: paired noise bouts
    if (n_restart_noise != n_end_noise) {
      file_errors$unpaired_noise <- tibble(
        error_type = "Unpaired Noise Annotations",
        filename   = filename,
        details    = paste(
          "restart_noise:", n_restart_noise,
          "| end_noise:", n_end_noise,
          "— each end_noise (3,1,9) must be followed by a restart_noise (1,9,9)"
        )
      )
    }
    
    # Check 7: noise bout ordering and overlaps
    if (n_restart_noise > 0 && n_restart_noise == n_end_noise) {
      
      restart_noise <- start_annotations %>%
        filter(annotation_order > 1) %>%
        mutate(noise_bout = row_number())
      
      noise_bouts <- end_noise %>%
        select(noise_bout, end_noise_time = begin_time_s) %>%
        left_join(
          restart_noise %>% select(noise_bout, restart_noise_time = begin_time_s),
          by = "noise_bout"
        ) %>%
        mutate(duration = restart_noise_time - end_noise_time)
      
      bad_order <- noise_bouts %>% filter(restart_noise_time <= end_noise_time)
      if (nrow(bad_order) > 0) {
        file_errors$noise_order <- tibble(
          error_type = "Invalid Noise Bout Order",
          filename   = filename,
          details    = paste("Bout(s)", paste(bad_order$noise_bout, collapse = ", "),
                             "have restart_noise at or before end_noise")
        )
      }
      
      if (nrow(noise_bouts) > 1) {
        overlapping <- which(noise_bouts$end_noise_time[-1] < noise_bouts$restart_noise_time[-nrow(noise_bouts)])
        if (length(overlapping) > 0) {
          file_errors$noise_overlap <- tibble(
            error_type = "Overlapping Noise Bouts",
            filename   = filename,
            details    = paste("Overlaps between bouts:",
                               paste(overlapping, overlapping + 1, sep = "&", collapse = "; "))
          )
        }
      }
    }
    
    # Check 8: effort temporal order
    if (nrow(start_annotations) >= 1 && nrow(end_effort_candidates) == 1) {
      if (end_effort_candidates$begin_time_s[1] <= start_annotations$begin_time_s[1]) {
        file_errors$effort_order <- tibble(
          error_type = "Invalid Effort Order",
          filename   = filename,
          details    = "end_effort comes at or before start_effort"
        )
      }
    }
    
    # Store results
    if (length(file_errors) > 0) errors_list[[filename]] <- bind_rows(file_errors)
    
    file_summary[[filename]] <- tibble(
      filename             = filename,
      total_rows           = nrow(df),
      start_effort_count   = as.integer(nrow(start_annotations) >= 1),
      restart_noise_count  = as.integer(n_restart_noise),
      end_noise_count      = as.integer(n_end_noise),
      end_effort_count     = as.integer(nrow(end_effort_candidates)),
      has_errors           = length(file_errors) > 0
    )
  }
  
  # ── Compile validation results ────────────────────────────────────────────
  all_errors    <- bind_rows(errors_list)
  all_summaries <- bind_rows(file_summary)
  
  cat("\n==================== VALIDATION SUMMARY ====================\n")
  cat("Total files checked:    ", length(txt_files), "\n")
  cat("Files with errors:      ", sum(all_summaries$has_errors, na.rm = TRUE), "\n")
  cat("Files passing all checks:", sum(!all_summaries$has_errors, na.rm = TRUE), "\n")
  
  if (nrow(all_errors) > 0) {
    cat("\n==================== ERRORS BY TYPE ====================\n")
    all_errors %>%
      count(error_type, sort = TRUE) %>%
      print()
    cat("\n==================== DETAILED ERRORS ====================\n")
    print(all_errors, n = Inf)
  } else {
    cat("\n✓ All files passed validation!\n")
  }
  
  cat("\n==================== FILE SUMMARY ====================\n")
  print(all_summaries, n = Inf)
  
  # ── Merge ─────────────────────────────────────────────────────────────────
  merged_df <- NULL
  
  if (merge) {
    
    if (!merge_on_errors && nrow(all_errors) > 0) {
      message("\nMerge skipped: validation errors found and merge_on_errors = FALSE.")
    } else {
      
      files_to_merge <- txt_files
      
      if (!merge_on_errors) {
        # Only keep files that passed validation
        passing_files  <- all_summaries %>% filter(!has_errors) %>% pull(filename)
        files_to_merge <- txt_files[basename(txt_files) %in% passing_files]
        message("\nMerging ", length(files_to_merge), " file(s) that passed validation ",
                "(", length(txt_files) - length(files_to_merge), " skipped due to errors).")
      } else {
        message("\nMerging all ", length(files_to_merge), " file(s).")
      }
      
      process_file <- function(file_path) {
        fname      <- basename(file_path)
        parts      <- str_split(fname, "_")[[1]]
        date_str   <- parts[3]
        time_str   <- str_extract(parts[4], "^\\d{4}")
        
        read_delim(file_path, delim = "\t", show_col_types = FALSE) %>%
          clean_names() %>%
          filter(nrow(.) > 0) %>%
          mutate(
            filename        = fname,
            cruise          = parts[1],
            dat_number = as.numeric(str_remove(parts[2], "SBDAT")),
            date            = as.POSIXct(paste(date_str, time_str),
                                         format = "%y%m%d %H%M",
                                         tz     = time_zone)
          )
      }
      
      merged_df <- map_df(files_to_merge, process_file)
      
      if (!is.null(output_file)) {
        saveRDS(merged_df, file = output_file)
        message("Merged data saved to: ", output_file)
      }
    }
  }
  
  # ── Return ────────────────────────────────────────────────────────────────
  invisible(list(
    errors    = all_errors,
    summary   = all_summaries,
    has_errors = nrow(all_errors) > 0,
    data      = merged_df
  ))
}
