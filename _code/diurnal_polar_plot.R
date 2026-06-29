# ============================================================
# diurnal_polar_plot.R
#
# Exports one function:
#   plot_diurnal_calls(df, ...)
#
# Required packages: tidyverse, janitor, suncalc, lubridate, patchwork
# ============================================================


# ------------------------------------------------------------
# INTERNAL HELPER: distribute a single recording's effort
# across the clock-hours it spans, returning one row per hour.
# ------------------------------------------------------------
.distribute_effort <- function(start_dt, end_dt,
                                be_det, be500_det,
                                sunrise, sunset, dawn, dusk) {

  hour_start <- lubridate::floor_date(start_dt, "hour")
  hour_end   <- lubridate::floor_date(end_dt,   "hour")
  hours      <- seq(hour_start, hour_end, by = "hour")

  purrr::map_dfr(hours, function(h) {
    seg_start <- max(start_dt, h)
    seg_end   <- min(end_dt,   h + lubridate::hours(1))
    mins      <- as.numeric(difftime(seg_end, seg_start, units = "mins"))
    if (mins <= 0) return(NULL)
    tibble::tibble(
      hour_of_day = lubridate::hour(h),
      effort_seg  = mins,
      be_det      = be_det,
      be500_det   = be500_det,
      sunrise     = sunrise,
      sunset      = sunset,
      dawn        = dawn,
      dusk        = dusk
    )
  })
}


# ------------------------------------------------------------
# INTERNAL HELPER: build one polar panel
# ------------------------------------------------------------
.make_polar_panel <- function(hourly, det_col, title_str, fill_color,
                               med_dawn, med_sunrise, med_sunset, med_dusk) {

  det_col_sym <- rlang::sym(det_col)
  max_det     <- max(hourly$be_detections, hourly$be500_detections, na.rm = TRUE)
  max_eff     <- max(hourly$effort_total_min, na.rm = TRUE)
  eff_scale   <- if (max_eff > 0) max_det / max_eff else 1

  shade_bands <- dplyr::bind_rows(
    tibble::tibble(xmin = med_sunset, xmax = med_dusk,  ymin = 0, ymax = Inf, fill = "lightyellow"),  # evening twilight
    tibble::tibble(xmin = med_dawn,   xmax = med_sunrise,ymin = 0, ymax = Inf, fill = "lightyellow"),  # morning twilight
    tibble::tibble(xmin = med_dusk,   xmax = 24,         ymin = 0, ymax = Inf, fill = "grey70"),       # night: dusk -> midnight
    tibble::tibble(xmin = 0,          xmax = med_dawn,   ymin = 0, ymax = Inf, fill = "grey70")        # night: midnight -> dawn
  )

  ggplot2::ggplot(hourly, ggplot2::aes(x = hour_of_day + 0.5)) +

    # Background shading
    ggplot2::geom_rect(
      data        = shade_bands,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
      alpha       = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_identity() +

    # Effort outline bars
    ggplot2::geom_col(
      ggplot2::aes(y = effort_total_min * eff_scale),
      fill      = NA,
      color     = "black",
      width     = 1,
      linewidth = 0.4
    ) +

    # Detection filled bars
    ggplot2::geom_col(
      ggplot2::aes(y = !!det_col_sym),
      fill  = fill_color,
      color = NA,
      alpha = 0.85,
      width = 1
    ) +

    ggplot2::coord_polar(theta = "x", start = 0, direction = 1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = 0:23,
      labels = sprintf("%02d:00", 0:23)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = title_str, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 13),
      axis.text.x      = ggplot2::element_text(size = 7.5, color = "grey30"),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position  = "none"
    )
}


# ------------------------------------------------------------
# MAIN EXPORTED FUNCTION
# ------------------------------------------------------------
#' Plot diurnal variation in whale call detections and sonobuoy effort
#'
#' @param df            Data frame containing the sonobuoy dataset. Column names
#'                      are cleaned with janitor::clean_names() internally, so
#'                      raw mixed-case names are fine.
#' @param date_col      Name of the column holding recording start datetime
#'                      (character or POSIXct). Default: "date".
#' @param effort_col    Name of the column holding effort duration in minutes.
#'                      Default: "effort_min".
#' @param lat_col       Name of the latitude column. Default: "latitude".
#' @param lon_col       Name of the longitude column. Default: "longitude".
#' @param be_col        Name of the BE detection column. Default: "be".
#' @param be500_col     Name of the BE500 detection column. Default: "be500".
#' @param be_color      Fill color for BE detection bars. Default: "#2166AC".
#' @param be500_color   Fill color for BE500 detection bars. Default: "#B2182B".
#' @param be_title      Panel title for BE. Default: "Blue Whale 20-Hz (BE) Calls".
#' @param be500_title   Panel title for BE500. Default: "Blue Whale 500-Hz (BE500) Calls".
#' @param plot_title    Overall figure title.
#' @param save_path     If non-NULL, path to save the PNG (e.g. here::here("plot.png")).
#'                      Default: NULL (no file saved).
#' @param width         Plot width in inches (used only when save_path is set). Default: 12.
#' @param height        Plot height in inches. Default: 6.5.
#' @param dpi           Resolution for saved PNG. Default: 300.
#'
#' @return A patchwork object (two faceted polar plots). Printed invisibly if
#'         save_path is provided; returned visibly otherwise.
#'
#' @examples
#' \dontrun{
#' library(tidyverse); library(here)
#' df <- read_csv(here("sonobuoy_data.csv"))
#'
#' # Basic call — uses all defaults
#' plot_diurnal_calls(df)
#'
#' # Save to file
#' plot_diurnal_calls(df, save_path = here("figures", "diurnal_plot.png"))
#'
#' # Custom colors and titles
#' plot_diurnal_calls(
#'   df,
#'   be_color    = "steelblue",
#'   be500_color = "firebrick",
#'   plot_title  = "HICEAS 2002 — Diurnal Call Patterns"
#' )
#' }
plot_diurnal_calls <- function(
    df,
    date_col    = "date",
    effort_col  = "effort_min",
    lat_col     = "latitude",
    lon_col     = "longitude",
    be_col      = "be",
    be500_col   = "be500",
    be_color    = "#2166AC",
    be500_color = "#B2182B",
    be_title    = "Blue Whale 20-Hz (BE) Calls",
    be500_title = "Blue Whale 500-Hz (BE500) Calls",
    plot_title  = "Diurnal Variation in Blue Whale Call Detections & Sonobuoy Effort",
    save_path   = NULL,
    width       = 12,
    height      = 6.5,
    dpi         = 300
) {

  # --- 1. Clean names --------------------------------------------------------
  df <- janitor::clean_names(df)

  # Helper: fetch a column by user-supplied name (applied after clean_names)
  get_col <- function(d, col_arg) {
    clean <- janitor::make_clean_names(col_arg)
    if (!clean %in% names(d))
      stop(sprintf("Column '%s' (cleaned to '%s') not found in data. Available: %s",
                   col_arg, clean, paste(names(d), collapse = ", ")))
    d[[clean]]
  }

  # --- 2. Parse & filter -------------------------------------------------------
  # Extract columns by name BEFORE mutate so dplyr masking can't interfere
  start_vec    <- lubridate::ymd_hms(get_col(df, date_col),   quiet = TRUE)
  effort_vec   <- as.numeric(get_col(df, effort_col))
  lat_vec      <- as.numeric(get_col(df, lat_col))
  lon_vec      <- as.numeric(get_col(df, lon_col))
  be_vec       <- tidyr::replace_na(get_col(df, be_col)    == "TRUE", FALSE)
  be500_vec    <- tidyr::replace_na(get_col(df, be500_col) == "TRUE", FALSE)

  df <- dplyr::mutate(df,
    start_dt   = start_vec,
    effort_min = effort_vec,
    latitude   = lat_vec,
    longitude  = lon_vec,
    be_det     = be_vec,
    be500_det  = be500_vec,
    end_dt     = start_dt + lubridate::dminutes(effort_min)
  )

  df <- dplyr::filter(df,
    !is.na(start_dt),
    !is.na(effort_min), effort_min > 0,
    !is.na(latitude), !is.na(longitude)
  )

  if (nrow(df) == 0) stop("No valid rows remain after filtering. Check date, effort, and lat/lon columns.")

  # --- 3. Per-recording sunrise/sunset -----------------------------------------
  sun_df <- suncalc::getSunlightTimes(
    data = data.frame(
      date = as.Date(df$start_dt),
      lat  = df$latitude,
      lon  = df$longitude
    ),
    keep = c("sunrise", "sunset", "dawn", "dusk"),
    tz   = "UTC"
  )

  df <- dplyr::bind_cols(df, sun_df |> dplyr::select(sunrise, sunset, dawn, dusk))

  # --- 4. Distribute effort across hours ---------------------------------------
  effort_long <- df |>
    dplyr::select(start_dt, end_dt, be_det, be500_det, sunrise, sunset, dawn, dusk) |>
    purrr::pmap_dfr(~.distribute_effort(..1, ..2, ..3, ..4, ..5, ..6, ..7, ..8))

  # --- 5. Aggregate by hour ----------------------------------------------------
  hourly <- effort_long |>
    dplyr::group_by(hour_of_day) |>
    dplyr::summarise(
      effort_total_min = sum(effort_seg,  na.rm = TRUE),
      be_detections    = sum(be_det,      na.rm = TRUE),
      be500_detections = sum(be500_det,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::complete(
      hour_of_day = 0:23,
      fill = list(effort_total_min = 0, be_detections = 0, be500_detections = 0)
    )

  # --- 6. Median solar times for shading --------------------------------------
  med_time <- function(x) {
    median(lubridate::hour(x), na.rm = TRUE) +
      median(lubridate::minute(x), na.rm = TRUE) / 60
  }
  med_dawn    <- med_time(df$dawn)
  med_sunrise <- med_time(df$sunrise)
  med_sunset  <- med_time(df$sunset)
  med_dusk    <- med_time(df$dusk)

  # --- 7. Build panels ---------------------------------------------------------
  p_be <- .make_polar_panel(hourly, "be_detections",    be_title,    be_color,
                             med_dawn, med_sunrise, med_sunset, med_dusk)
  p_be500 <- .make_polar_panel(hourly, "be500_detections", be500_title, be500_color,
                                med_dawn, med_sunrise, med_sunset, med_dusk)

  # --- 8. Combine with patchwork -----------------------------------------------
  caption_txt <- sprintf(
    "Colored bars = recordings with detections  |  Black outline = total effort (min, scaled)  |  Shading based on median solar times: dawn %.1fh, sunrise %.1fh, sunset %.1fh, dusk %.1fh",
    med_dawn, med_sunrise, med_sunset, med_dusk
  )

  combined <- p_be + p_be500 +
    patchwork::plot_annotation(
      title   = plot_title,
      caption = caption_txt,
      theme   = ggplot2::theme(
        plot.title   = ggplot2::element_text(face = "bold", size = 15, hjust = 0.5),
        plot.caption = ggplot2::element_text(size = 7, color = "grey40", hjust = 0.5)
      )
    )

  # --- 9. Save if requested ----------------------------------------------------
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = width, height = height,
                   dpi = dpi, bg = "white")
    message("Saved: ", save_path)
    return(invisible(combined))
  }

  combined
}
