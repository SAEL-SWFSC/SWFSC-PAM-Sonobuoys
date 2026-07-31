# merge_sonobuoy_csv.R
# --------------------
# Merges multiple sonobuoy CSV files from a single directory into one
# combined CSV. Validates column consistency, standardises dates, detects
# duplicates, and sorts by Cruise then Date.
#
# Usage (from terminal):
#   Rscript merge_sonobuoy_csv.R <input_directory> <output_file>
#
# Example:
#   Rscript merge_sonobuoy_csv.R ./data ./merged_output.csv
#
# Or source interactively and call merge_sonobuoy_csv() directly:
#   source("merge_sonobuoy_csv.R")
#   merge_sonobuoy_csv(input_dir = "./data", output_file = "./merged_output.csv")
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(purrr)
  library(janitor)
})

# ── Constants ─────────────────────────────────────────────────────────────────

EXPECTED_COLUMNS <- c(
  "Cruise", "Sonobuoy Number", "Date", "Latitude", "Longitude",
  "Sighting Number", "Opportunistic Buoy?", "Sonobuoy Type", "Channel",
  "Depth", "Hours to Scuttle", "Bad Buoys", "Ship Noise?", "Difar Signal?",
  "Water Noise?", "Biological Sounds?", "Sounds Detected", "DAT Tape Number",
  "Additional Tapes?", "Reception Distance (nmi)", "Recording Reviewed?",
  "Comments"
)

# Formats tried in order. tz set to UTC as a neutral label for floating times —
# no conversion ever occurs; UTC is used purely to suppress offset adjustments.
DATE_FORMATS <- c(
  "%Y-%m-%d %H:%M:%S",   # 2000-07-31 09:47:00
  "%Y-%m-%d %H:%M",      # 2000-07-31 09:47
  "%m/%d/%Y %H:%M:%S",   # 08/15/2006 18:46:00
  "%m/%d/%Y %H:%M",      # 8/15/2006 18:46
  "%m/%d/%y %H:%M:%S",   # 08/15/06 18:46:00
  "%m/%d/%y %H:%M"       # 8/15/06 18:46
)

OUTPUT_DATE_FORMAT <- "%m/%d/%Y %H:%M"   # MM/DD/YYYY HH:MM  (24-hour)


# ── Helper: parse a single date string ───────────────────────────────────────

parse_date_value <- function(raw) {
  # Returns a POSIXct (tz = "UTC") or POSIXct NA.
  # tz is set to UTC as a neutral label for floating local times —
  # no timezone conversion ever occurs.
  posixct_na <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  if (is.na(raw) || str_trim(raw) == "") return(posixct_na)
  raw <- str_trim(raw)
  for (fmt in DATE_FORMATS) {
    result <- as.POSIXct(raw, format = fmt, tz = "UTC")
    if (!is.na(result)) return(result)
  }
  # Last-resort: lubridate guesser
  result <- suppressWarnings(
    parse_date_time(raw, orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM"),
                    tz = "UTC")
  )
  if (!is.na(result)) {
    message(sprintf("  ⚠  Guessed format for value: '%s'  →  %s  (verify this is correct)",
                    raw, format(result, OUTPUT_DATE_FORMAT, tz = "UTC")))
    return(result)
  }
  return(posixct_na)
}


# ── Helper: check columns of one data frame ──────────────────────────────────

check_columns <- function(df, filename) {
  actual   <- names(df)
  missing  <- setdiff(EXPECTED_COLUMNS, actual)
  extra    <- setdiff(actual, EXPECTED_COLUMNS)
  errors   <- character(0)
  if (length(missing) > 0)
    errors <- c(errors, sprintf("  MISSING columns : %s", paste(sort(missing), collapse = ", ")))
  if (length(extra) > 0)
    errors <- c(errors, sprintf("  EXTRA columns   : %s", paste(sort(extra),   collapse = ", ")))
  errors
}


# ── Main function ─────────────────────────────────────────────────────────────

merge_sonobuoy_csv <- function(input_dir, output_file) {

  # ── 1. Validate input directory ──────────────────────────────────────────
  if (!dir.exists(input_dir)) {
    stop("ERROR: Input directory not found: ", input_dir)
  }

  # ── 2. Discover CSV files (top-level only) ────────────────────────────────
  csv_files <- list.files(input_dir, pattern = "\\.csv$",
                          full.names = TRUE, recursive = FALSE)
  if (length(csv_files) == 0) {
    stop("ERROR: No CSV files found in: ", input_dir)
  }

  cat(sprintf("\nFound %d CSV file(s) in '%s':\n", length(csv_files), input_dir))
  cat(paste0("  ", basename(csv_files), "\n"))

  # ── 3. Read & validate each file ─────────────────────────────────────────
  frames        <- list()
  column_errors <- list()

  for (filepath in csv_files) {
    fname <- basename(filepath)
    df <- tryCatch(
      read_csv(filepath,
               col_types = cols(.default = col_character()),
               trim_ws   = TRUE,
               show_col_types = FALSE),
      error = function(e) stop(sprintf("ERROR reading '%s': %s", fname, e$message))
    )

    # Trim column names
    names(df) <- str_trim(names(df))

    errs <- check_columns(df, fname)
    if (length(errs) > 0) column_errors[[fname]] <- errs

    df[["_source_file"]] <- fname
    frames[[fname]] <- df
  }

  # ── 4. Halt on column mismatches ─────────────────────────────────────────
  if (length(column_errors) > 0) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("ERROR: Column mismatch detected — merge aborted.\n")
    cat(strrep("=", 60), "\n")
    for (fname in names(column_errors)) {
      cat(sprintf("\n  File: %s\n", fname))
      cat(column_errors[[fname]], sep = "\n")
    }
    cat("\nPlease fix the column issues above and re-run.\n")
    stop("Column mismatch — see details above.")
  }

  # ── 5. Concatenate ────────────────────────────────────────────────────────
  cat("\nAll column checks passed. Concatenating files ...\n")
  merged <- bind_rows(frames)

  # ── 6. Parse & standardise dates ─────────────────────────────────────────
  cat("Parsing dates ...\n")

  merged <- merged %>%
    mutate(
      date_original = str_trim(Date),
      parsed_date   = as.POSIXct(
                        map_dbl(date_original, ~ as.numeric(parse_date_value(.x))),
                        origin = "1970-01-01",
                        tz     = "UTC"
                      )
    )

  # Report unparseable dates
  bad_rows <- merged %>%
    filter(is.na(parsed_date) & date_original != "")

  if (nrow(bad_rows) > 0) {
    cat(sprintf(
      "\n⚠  WARNING: %d row(s) had unparseable Date values and will have a blank 'date' column:\n",
      nrow(bad_rows)
    ))
    bad_rows %>%
      select(`_source_file`, Cruise, `Sonobuoy Number`, date_original) %>%
      rename(File = `_source_file`, `Raw Date` = date_original) %>%
      print(n = Inf)
  }

  # Format to MM/DD/YYYY HH:MM  (24-hour, UTC label preserves floating times)
  merged <- merged %>%
    mutate(date = if_else(
      !is.na(parsed_date),
      format(parsed_date, OUTPUT_DATE_FORMAT, tz = "UTC"),
      NA_character_
    ))

  # ── 7. Duplicate detection ────────────────────────────────────────────────
  # Compare on all original source columns (not derived date/date_original)
  check_cols <- setdiff(EXPECTED_COLUMNS, character(0))   # all expected cols

  dup_flags <- merged %>%
    select(all_of(check_cols)) %>%
    duplicated(fromLast = FALSE) |
    merged %>%
      select(all_of(check_cols)) %>%
      duplicated(fromLast = TRUE)

  duplicates <- merged[dup_flags, ]

  if (nrow(duplicates) > 0) {
    cat("\n", strrep("!", 60), "\n", sep = "")
    cat(sprintf("⚠  WARNING: %d DUPLICATE ROWS DETECTED  ⚠\n", nrow(duplicates)))
    cat(strrep("!", 60), "\n")
    cat("These rows appear more than once (identical across all original columns):\n\n")

    duplicates %>%
      select(`_source_file`, Cruise, `Sonobuoy Number`, Date, Latitude, Longitude) %>%
      rename(File = `_source_file`) %>%
      print(n = Inf)

    # Save duplicate report
    dup_report <- sub("\\.csv$", "_DUPLICATES.csv", output_file)
    if (dup_report == output_file) dup_report <- paste0(output_file, "_DUPLICATES.csv")

    duplicates %>%
      select(-`_source_file`) %>%
      clean_names() %>%
      write_csv(dup_report, na = "")

    cat(sprintf("\nDuplicate rows saved to: %s\n", dup_report))
    cat("The merged file will INCLUDE all copies so you can review them.\n")
    cat(strrep("!", 60), "\n")
  }

  # ── 8. Sort by Cruise then Date ───────────────────────────────────────────
  cat("\nSorting by Cruise → Date ...\n")

  merged <- merged %>%
    mutate(Cruise_num = suppressWarnings(as.numeric(Cruise))) %>%
    arrange(Cruise_num, parsed_date) %>%
    select(-Cruise_num)

  # ── 9. Final column order ─────────────────────────────────────────────────
  # date_original and date replace the original Date column
  other_cols <- setdiff(EXPECTED_COLUMNS, "Date")
  final_cols <- c("date_original", "date", other_cols)

  merged <- merged %>%
    select(all_of(final_cols))

  # ── 10. Clean column names to snake_case ──────────────────────────────────
  names_before <- names(merged)
  merged       <- clean_names(merged)
  names_after  <- names(merged)

  # ── 11. Save ───────────────────────────────────────────────────────────────
  write_csv(merged, output_file, na = "")

  cat(sprintf("\n✓  Merged file saved: %s\n", output_file))
  cat(sprintf("   Total rows : %d\n", nrow(merged)))
  cat(sprintf("   Total cols : %d\n", ncol(merged)))
  cat("\nColumn names after clean_names() — original → snake_case:\n")
  name_map <- data.frame(
    num      = sprintf("  %2d.", seq_along(names_before)),
    original = names_before,
    arrow    = "→",
    clean    = names_after,
    check.names = FALSE
  )
  print(name_map, row.names = FALSE)

  invisible(merged)
}


# ── Run from command line ─────────────────────────────────────────────────────

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2) {
    cat("Usage: Rscript merge_sonobuoy_csv.R <input_directory> <output_file>\n")
    quit(status = 1)
  }
  merge_sonobuoy_csv(input_dir = args[1], output_file = args[2])
}

