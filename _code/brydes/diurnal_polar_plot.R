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
# INTERNAL HELPER: classify an hour (0-23) into day/dawn/night
# given solar transition times (all in decimal hours, 0-24)
# ------------------------------------------------------------
.hour_period <- function(h, dawn, sunrise, sunset, dusk) {
  mid <- h + 0.5  # midpoint of the hour slot
  if      (mid >= sunrise && mid < sunset)  "day"
  else if (mid >= dawn    && mid < sunrise) "dawn"
  else if (mid >= sunset  && mid < dusk)    "dawn"
  else                                       "night"
}

# ------------------------------------------------------------
# INTERNAL HELPER: build one polar panel
#
# Shading strategy: assign each of the 24 hour-slots a period
# (day/dawn/night) and draw a full-height geom_col behind the
# data bars. This is the only approach that works correctly
# after coord_polar(), because annotate("rect") uses
# pre-transform Cartesian coordinates and ends up misaligned.
#
# Parameters:
#   hourly         - aggregated hourly data frame
#   det_col        - detection count column name
#   title_str      - panel title
#   fill_color     - detection bar fill color
#   med_dawn/sunrise/sunset/dusk - solar times in decimal hours (0-24)
#   day_color      - color for daytime slots   (NA = transparent)
#   dawn_color     - color for twilight slots
#   night_color    - color for night slots
#   global_max_eff - if non-NULL, shared effort scale denominator
# ------------------------------------------------------------
.make_polar_panel <- function(hourly, det_col, title_str, fill_color,
                               med_dawn, med_sunrise, med_sunset, med_dusk,
                               day_color       = NA,
                               dawn_color      = "#FFB6C1",
                               night_color     = "grey70",
                               global_max_eff  = NULL,
                               show_det_labels = TRUE) {

  det_col_sym <- rlang::sym(det_col)

  # --- Scales ------------------------------------------------------------------
  max_det <- max(hourly[[det_col]], na.rm = TRUE)
  if (max_det == 0) max_det <- 1

  max_eff   <- if (!is.null(global_max_eff)) global_max_eff else max(hourly$effort_total_min, na.rm = TRUE)
  if (max_eff == 0) max_eff <- 1
  eff_scale <- max_det / max_eff

  # y scale upper limit — shade bars are drawn to exactly this value,
  # and the axis is pinned here so shading fills the circle without
  # pushing data bars to microscopic size (the old shade_ymax * 50 bug).
  # Add 10% headroom above max_det so labels fit outside bar tips.
  y_max <- max(max_det, max_eff * eff_scale) * 1.15

  # --- Assign each hour a period color -----------------------------------------
  period_colors <- sapply(0:23, function(h) {
    period <- .hour_period(h, med_dawn, med_sunrise, med_sunset, med_dusk)
    switch(period,
      day   = if (is.na(day_color)) NA_character_ else day_color,
      dawn  = dawn_color,
      night = night_color
    )
  })

  shade_df <- tibble::tibble(
    hour_of_day  = 0:23,
    x_pos        = 0:23 + 0.5,
    period_color = period_colors
  )

  # --- Detection label data ----------------------------------------------------
  label_data <- hourly |>
    dplyr::filter(!!det_col_sym > 0) |>
    dplyr::mutate(
      x_pos = hour_of_day + 0.5,
      y_pos = !!det_col_sym + max_det * 0.06,
      label = as.character(!!det_col_sym)
    )

  # --- Build plot --------------------------------------------------------------
  p <- ggplot2::ggplot(hourly, ggplot2::aes(x = hour_of_day + 0.5))

  # Shading: one bar per hour drawn to y_max (same as axis limit),
  # so it fills the full radial extent without distorting the scale.
  # NA period_color (day_color = NA) slots are skipped entirely.
  shade_df_draw <- shade_df[!is.na(shade_df$period_color), ]
  if (nrow(shade_df_draw) > 0) {
    p <- p + ggplot2::geom_col(
      data  = shade_df_draw,
      ggplot2::aes(x = x_pos, y = y_max),
      fill  = shade_df_draw$period_color,
      color = NA,
      width = 1,
      inherit.aes = FALSE
    )
  }

  p <- p +
    # --- Detection filled bars (below effort outline) ---
    ggplot2::geom_col(
      ggplot2::aes(y = !!det_col_sym),
      fill  = fill_color,
      color = NA,
      alpha = 0.85,
      width = 1
    ) +

    # --- Effort outline bars (on top) ---
    ggplot2::geom_col(
      ggplot2::aes(y = effort_total_min * eff_scale),
      fill      = NA,
      color     = "black",
      width     = 1,
      linewidth = 0.4
    ) +

    # --- Detection count labels at bar tips (optional) ---
    { if (show_det_labels)
        ggplot2::geom_text(
          data        = label_data,
          ggplot2::aes(x = x_pos, y = y_pos, label = label),
          size        = 2.8,
          fontface    = "bold",
          family      = "Times New Roman",
          color       = "black",
          inherit.aes = FALSE
        )
      else
        ggplot2::geom_blank()
    } +

    ggplot2::coord_polar(theta = "x", start = 0, direction = 1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 24),
      breaks = 0:23,
      labels = sprintf("%02d:00", 0:23)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::labs(title = title_str, x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11, base_family = "Times New Roman") +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 13, family = "Times New Roman"),
      axis.text.x      = ggplot2::element_text(size = 7.5, color = "grey30", family = "Times New Roman"),
      axis.text.y      = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position  = "none"
    )

  p
}


# ------------------------------------------------------------
# MAIN EXPORTED FUNCTION
# ------------------------------------------------------------
#' Plot diurnal variation in whale call detections and sonobuoy effort
#'
#' @param df               Data frame containing the sonobuoy dataset.
#' @param date_col         Column holding recording start datetime. Default: "date".
#' @param effort_col       Column holding effort duration in minutes. Default: "effort_min".
#' @param lat_col          Latitude column. Default: "latitude".
#' @param lon_col          Longitude column. Default: "longitude".
#' @param be_col           BE detection column. Default: "be".
#' @param be500_col        BE500 detection column. Default: "be500".
#' @param be_color         Fill color for BE bars. Default: "#2166AC".
#' @param be500_color      Fill color for BE500 bars. Default: "#B2182B".
#' @param be_title         Panel title for BE.
#' @param be500_title      Panel title for BE500.
#' @param plot_title       Overall figure title.
#' @param day_color        Background color for daytime. Default: NA (transparent/no fill).
#'                         Use "white" or any color string to add a fill.
#' @param dawn_color       Background color for dawn/dusk twilight. Default: "#FFB6C1" (light pink).
#' @param night_color      Background color for night. Default: "grey70".
#' @param shared_effort_scale  Logical. If TRUE, effort bars use the same scale across
#'                             both panels (so effort height is directly comparable).
#'                             If FALSE (default), each panel scales effort independently
#'                             to its own detection maximum.
#' @param force_dawn       Override the suncalc-derived dawn time with a fixed decimal hour
#'                         (e.g. force_dawn = 5.5 for 05:30). Default: NULL (use suncalc median).
#' @param force_sunrise    Override sunrise. Default: NULL.
#' @param force_sunset     Override sunset. Default: NULL.
#' @param force_dusk       Override dusk. Default: NULL.
#' @param show_det_labels  Logical. If TRUE (default), prints the recording-hour
#'                         detection count at the tip of each bar. Set FALSE to
#'                         hide labels (e.g. when counts are misleading because
#'                         recordings span multiple hour bins).
#' @param save_path        Path to save PNG. Default: NULL (no file saved).
#' @param width            Plot width in inches. Default: 12.
#' @param height           Plot height in inches. Default: 6.5.
#' @param dpi              Resolution for saved PNG. Default: 300.
#'
#' @return A patchwork object (two polar plots). Printed invisibly if save_path
#'         is provided; returned visibly otherwise.
#'
#' @examples
#' \dontrun{
#' library(tidyverse); library(here)
#' df <- read_csv(here("sonobuoy_data.csv"))
#'
#' # Basic — uses all defaults, independent effort scaling
#' plot_diurnal_calls(df)
#'
#' # Shared effort scale so both panels are directly comparable
#' plot_diurnal_calls(df, shared_effort_scale = TRUE)
#'
#' # Custom shading colors (day_color = NA means transparent/no daytime fill)
#' plot_diurnal_calls(df, day_color = NA, dawn_color = "#FFB6C1", night_color = "grey60")
#'
#' # Force fixed solar times instead of computing from lat/lon
#' plot_diurnal_calls(df, force_dawn = 5.5, force_sunrise = 6.25, force_sunset = 19.5, force_dusk = 20.25)
#'
#' # Save
#' plot_diurnal_calls(df, save_path = here("figures", "diurnal_plot.png"))
#' }
plot_diurnal_calls <- function(
    df,
    date_col            = "date",
    effort_col          = "effort_min",
    lat_col             = "latitude",
    lon_col             = "longitude",
    be_col              = "be",
    be500_col           = "be500",
    be_color            = "#2166AC",
    be500_color         = "#B2182B",
    be_title            = "Blue Whale 20-Hz (BE) Calls",
    be500_title         = "Blue Whale 500-Hz (BE500) Calls",
    plot_title          = "Diurnal Variation in Blue Whale Call Detections & Sonobuoy Effort",
    day_color           = NA,        # NA = transparent (no fill); use "white" or any color string
    dawn_color          = "#FFB6C1", # light pink
    night_color         = "grey70",
    shared_effort_scale = FALSE,
    force_dawn          = NULL,      # override suncalc median, e.g. force_dawn = 5.5 (5:30 AM)
    force_sunrise       = NULL,
    force_sunset        = NULL,
    force_dusk          = NULL,
    show_det_labels     = TRUE,  # set FALSE to hide recording-hour counts on bar tips
    save_path           = NULL,
    width               = 12,
    height              = 6.5,
    dpi                 = 300
) {

  # --- 1. Clean names --------------------------------------------------------
  df <- janitor::clean_names(df)

  # Helper: fetch a column by user-supplied name (after clean_names normalisation)
  get_col <- function(d, col_arg) {
    clean <- janitor::make_clean_names(col_arg)
    if (!clean %in% names(d))
      stop(sprintf("Column '%s' (cleaned to '%s') not found in data. Available: %s",
                   col_arg, clean, paste(names(d), collapse = ", ")))
    d[[clean]]
  }

  # --- 2. Parse & filter -------------------------------------------------------
  start_vec  <- lubridate::ymd_hms(get_col(df, date_col), quiet = TRUE)
  effort_vec <- suppressWarnings(as.numeric(get_col(df, effort_col)))
  lat_vec    <- suppressWarnings(as.numeric(get_col(df, lat_col)))
  lon_vec    <- suppressWarnings(as.numeric(get_col(df, lon_col)))
  # be: TRUE/FALSE strings — only "TRUE" counts as a detection
  be_vec     <- tidyr::replace_na(get_col(df, be_col) == "TRUE", FALSE)

  # be500: numeric count column — any value > 0 is a detection
  be500_raw  <- suppressWarnings(as.numeric(get_col(df, be500_col)))
  be500_vec  <- tidyr::replace_na(be500_raw > 0, FALSE)

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

  if (nrow(df) == 0)
    stop("No valid rows remain after filtering. Check date, effort, and lat/lon columns.")

  # --- 3. Per-recording sunrise/sunset -----------------------------------------
  # suncalc returns POSIXct in UTC. We need solar event times as decimal hours
  # in the *same reference frame as the hour-of-day axis* (i.e. the timezone
  # of start_dt). Detect the timezone from start_dt and convert accordingly.
  # If start_dt has no tz (or is UTC), solar times stay in UTC — which is only
  # correct if your recording times are also UTC.
  rec_tz <- lubridate::tz(df$start_dt)
  if (is.null(rec_tz) || rec_tz == "") rec_tz <- "UTC"

  sun_df <- suncalc::getSunlightTimes(
    data = data.frame(
      date = as.Date(df$start_dt, tz = rec_tz),
      lat  = df$latitude,
      lon  = df$longitude
    ),
    keep = c("sunrise", "sunset", "dawn", "dusk"),
    tz   = rec_tz   # return solar times in the same tz as the recordings
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

  # --- 6. Solar times for shading ---------------------------------------------
  # Decimal hour in the recording timezone (same as hour_of_day axis).
  to_decimal_hour <- function(x) lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600

  # Allow user to force solar times (skip suncalc medians)
  med_dawn    <- if (!is.null(force_dawn))    force_dawn    else median(to_decimal_hour(df$dawn),    na.rm = TRUE)
  med_sunrise <- if (!is.null(force_sunrise)) force_sunrise else median(to_decimal_hour(df$sunrise), na.rm = TRUE)
  med_sunset  <- if (!is.null(force_sunset))  force_sunset  else median(to_decimal_hour(df$sunset),  na.rm = TRUE)
  med_dusk    <- if (!is.null(force_dusk))    force_dusk    else median(to_decimal_hour(df$dusk),    na.rm = TRUE)

  message(sprintf(
    "Recording tz: %s | Solar shading (decimal hr): dawn=%.2f, sunrise=%.2f, sunset=%.2f, dusk=%.2f",
    rec_tz, med_dawn, med_sunrise, med_sunset, med_dusk
  ))

  # --- 7. Effort scale ---------------------------------------------------------
  # If shared_effort_scale = TRUE, both panels use the same effort denominator
  global_eff <- if (shared_effort_scale) max(hourly$effort_total_min, na.rm = TRUE) else NULL

  # --- 8. Build panels ---------------------------------------------------------
  p_be <- .make_polar_panel(
    hourly, "be_detections", be_title, be_color,
    med_dawn, med_sunrise, med_sunset, med_dusk,
    day_color, dawn_color, night_color,
    global_max_eff  = global_eff,
    show_det_labels = show_det_labels
  )

  p_be500 <- .make_polar_panel(
    hourly, "be500_detections", be500_title, be500_color,
    med_dawn, med_sunrise, med_sunset, med_dusk,
    day_color, dawn_color, night_color,
    global_max_eff  = global_eff,
    show_det_labels = show_det_labels
  )

  # --- 9. Combine with patchwork (no caption) ----------------------------------
  combined <- p_be + p_be500 +
    patchwork::plot_annotation(
      title = plot_title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold", size = 15, hjust = 0.5, family = "Times New Roman"
        )
      )
    )

  # --- 10. Save if requested ---------------------------------------------------
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = width, height = height,
                   dpi = dpi, bg = "white")
    message("Saved: ", save_path)
    return(invisible(combined))
  }

  combined
}
