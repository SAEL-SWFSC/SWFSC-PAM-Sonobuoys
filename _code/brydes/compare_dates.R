# compare_dates.R
# ----------------
# Compares the date column between two user-defined dataframes.
# Sorts each dataframe by the specified date column before comparing.
# Returns "Dates Match!" if all dates are equal, otherwise returns a
# detailed error object.
#
# Usage:
#   source("compare_dates.R")
#   result <- compare_dates(df1, df2, date_col = "date",
#                           df1_name = "df1", df2_name = "df2")
#
# Arguments:
#   df1, df2    : dataframes to compare
#   date_col    : name of the date column (must exist in both dataframes)
#   df1_name    : label used for df1 in the error output  (default: "df1")
#   df2_name    : label used for df2 in the error output  (default: "df2")
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(purrr)
})

# ── Date formats tried in order ───────────────────────────────────────────────
DATE_FORMATS <- c(
  "%Y-%m-%d %H:%M:%S",
  "%Y-%m-%d %H:%M",
  "%m/%d/%Y %H:%M:%S",
  "%m/%d/%Y %H:%M",
  "%m/%d/%y %H:%M:%S",
  "%m/%d/%y %H:%M"
)

# ── Helper: parse a single date string to POSIXct (no timezone) ──────────────
parse_date_value <- function(raw) {
  if (is.na(raw) || str_trim(raw) == "") return(as.POSIXct(NA, tz = "UTC"))
  raw <- str_trim(raw)
  for (fmt in DATE_FORMATS) {
    result <- as.POSIXct(raw, format = fmt, tz = "UTC")
    if (!is.na(result)) return(result)
  }
  # Last-resort: lubridate guesser
  result <- suppressWarnings(
    parse_date_time(raw, orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM"), tz = "UTC")
  )
  if (!is.na(result)) {
    message(sprintf("  ⚠  Guessed format for: '%s' → %s  (verify this is correct)", raw, result))
    return(result)
  }
  return(as.POSIXct(NA, tz = "UTC"))
}

# ── Helper: parse a vector of date strings to POSIXct ────────────────────────
parse_date_vector <- function(x) {
  map(x, parse_date_value) %>% do.call(c, .)
}


# ── Main function ─────────────────────────────────────────────────────────────
compare_dates <- function(df1, df2,
                          date_col  = "date",
                          df1_name  = "df1",
                          df2_name  = "df2") {

  # ── 1. Validate: date column exists in both dataframes ─────────────────────
  missing_in <- c(
    if (!date_col %in% names(df1)) df1_name,
    if (!date_col %in% names(df2)) df2_name
  )
  if (length(missing_in) > 0) {
    stop(sprintf(
      "Column '%s' not found in: %s",
      date_col, paste(missing_in, collapse = ", ")
    ))
  }

  # ── 2. Parse date columns ──────────────────────────────────────────────────
  parsed1 <- parse_date_vector(df1[[date_col]])
  parsed2 <- parse_date_vector(df2[[date_col]])

  # ── 3. Sort each dataframe by parsed date ──────────────────────────────────
  order1 <- order(parsed1, na.last = TRUE)
  order2 <- order(parsed2, na.last = TRUE)

  df1_sorted     <- df1[order1, ]
  df2_sorted     <- df2[order2, ]
  parsed1_sorted <- parsed1[order1]
  parsed2_sorted <- parsed2[order2]

  # ── 4. Check for row count mismatch ───────────────────────────────────────
  n1 <- nrow(df1_sorted)
  n2 <- nrow(df2_sorted)

  row_mismatch <- n1 != n2
  if (row_mismatch) {
    message(sprintf(
      "\n⚠  Row count mismatch: '%s' has %d rows, '%s' has %d rows.",
      df1_name, n1, df2_name, n2
    ))
    message("   Comparing the first ", min(n1, n2), " rows positionally.\n")
  }

  # Align to the shorter dataframe for positional comparison
  n_compare <- min(n1, n2)
  p1 <- parsed1_sorted[seq_len(n_compare)]
  p2 <- parsed2_sorted[seq_len(n_compare)]
  raw1 <- df1_sorted[[date_col]][seq_len(n_compare)]
  raw2 <- df2_sorted[[date_col]][seq_len(n_compare)]

  # ── 5. Compare parsed values ───────────────────────────────────────────────
  # NA == NA  →  mismatch  (per spec)
  matches <- mapply(function(a, b) {
    if (is.na(a) || is.na(b)) return(FALSE)   # NA is always a mismatch
    as.numeric(a) == as.numeric(b)             # compare as epoch seconds
  }, p1, p2)

  # Any extra rows from the longer dataframe are also mismatches
  n_extra     <- abs(n1 - n2)
  any_mismatch <- any(!matches) || row_mismatch

  # ── 6. Return result ───────────────────────────────────────────────────────
  if (!any_mismatch) {
    return("Dates Match!")
  }

  # Build the full aligned error table (all rows, not just mismatches)
  # Pad the shorter side with NA if row counts differ
  max_n  <- max(n1, n2)
  col1   <- c(df1_sorted[[date_col]], rep(NA_character_, max_n - n1))
  col2   <- c(df2_sorted[[date_col]], rep(NA_character_, max_n - n2))

  # Rebuild match flags over the full aligned length
  p1_full <- c(parsed1_sorted, rep(as.POSIXct(NA, tz = "UTC"), max_n - n1))
  p2_full <- c(parsed2_sorted, rep(as.POSIXct(NA, tz = "UTC"), max_n - n2))

  match_full <- mapply(function(a, b) {
    if (is.na(a) || is.na(b)) return(FALSE)
    as.numeric(a) == as.numeric(b)
  }, p1_full, p2_full)

  error_tbl <- data.frame(
    row         = seq_len(max_n),
    match       = match_full,
    stringsAsFactors = FALSE
  )
  error_tbl[[df1_name]] <- col1
  error_tbl[[df2_name]] <- col2

  # Summary counts
  n_mismatched_rows <- sum(!match_full)
  n_na_rows         <- sum(
    mapply(function(a, b) is.na(a) || is.na(b), p1_full, p2_full)
  )

  # ── 7. Print a human-readable summary ─────────────────────────────────────
  cat(strrep("!", 60), "\n")
  cat("⚠  DATE MISMATCH DETECTED\n")
  cat(strrep("!", 60), "\n\n")

  if (row_mismatch) {
    cat(sprintf("  Row count  : '%s' = %d rows  |  '%s' = %d rows\n",
                df1_name, n1, df2_name, n2))
  }
  cat(sprintf("  Total rows compared  : %d\n", max_n))
  cat(sprintf("  Mismatched rows      : %d\n", n_mismatched_rows))
  cat(sprintf("  Rows with NA date    : %d\n", n_na_rows))
  cat(sprintf("  Matching rows        : %d\n\n", sum(match_full)))

  cat("Full aligned date comparison (all rows):\n")
  print(error_tbl, row.names = FALSE)
  cat("\n")
  cat("  'match = FALSE' rows include: value differences, NAs, and\n")
  cat("  unmatched rows where one dataframe is longer than the other.\n")
  cat(strrep("!", 60), "\n")

  # Return the error object invisibly so it can be captured and inspected
  invisible(list(
    matched          = FALSE,
    row_count_match  = !row_mismatch,
    n_df1            = n1,
    n_df2            = n2,
    n_mismatched     = n_mismatched_rows,
    n_na             = n_na_rows,
    comparison_table = error_tbl
  ))
}
