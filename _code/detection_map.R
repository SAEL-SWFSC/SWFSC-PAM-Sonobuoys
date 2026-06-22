# ============================================================
# detection_map.R
# Creates a publication-quality detection map with:
#   - GEBCO 15-arc-second bathymetry (light colors)
#   - Tan land with black outline
#   - Detection points colored by 'be500' / 'be' status
# ============================================================

# Required packages:
#   tidyverse, here, sf, terra, marmap, ggplot2,
#   ggspatial, rnaturalearth, rnaturalearthdata, scales

# Install if needed:
# install.packages(c("tidyverse", "here", "sf", "terra", "marmap",
#                    "ggspatial", "rnaturalearth", "rnaturalearthdata", "scales"))

library(tidyverse)
library(here)
library(sf)
library(terra)
library(marmap)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ----------------------------------------------------------
# Helper: auto-size points relative to map extent
# ----------------------------------------------------------
.auto_point_size <- function(lon_range, lat_range, n_points) {
  diag_deg      <- sqrt(lon_range^2 + lat_range^2)
  base          <- diag_deg * 0.08
  density_shrink <- max(0.4, 1 - log10(max(n_points, 1)) * 0.18)
  size          <- base * density_shrink
  size          <- max(1.2, min(size, 6))
  return(size)
}

# ----------------------------------------------------------
# Main function
# ----------------------------------------------------------
#
# Args:
#   detection_df : data frame with columns: latitude, longitude, be, be500
#   output_name  : filename stem (no extension) saved to here("output/")
#   point_size   : numeric pt size for circles, or "auto" (default)
#   padding_deg  : degrees of padding around data extent (default 1)
#   xlim         : c(lon_min, lon_max) to override auto extent (optional)
#   ylim         : c(lat_min, lat_max) to override auto extent (optional)
#   dpi          : output resolution in DPI (default 300)
#   fig_width    : figure width in inches (default 10)
#   fig_height   : figure height in inches (default 8)
#   bathy_res    : resolution in arc-minutes for marmap download (default 1,
#                  ~1 arc-minute; use 0.25 for finer but slower 15-arc-sec)
#   keep_bathy   : cache downloaded bathy as RDS in output/ for reuse (default TRUE)
#
detection_map <- function(
    detection_df,
    output_name,
    point_size  = "auto",
    padding_deg = 1,
    xlim        = NULL,   # c(lon_min, lon_max) — overrides auto-fit
    ylim        = NULL,   # c(lat_min, lat_max) — overrides auto-fit
    dpi         = 300,
    fig_width   = 10,
    fig_height  = 8,
    bathy_res   = 1,      # 1 arc-min default; use 0.25 for 15-arc-sec (slower)
    keep_bathy  = TRUE
) {

  # ---- 0. Validate column names --------------------------------------------
  required_cols <- c("latitude", "longitude", "be", "be500")
  missing_cols  <- setdiff(required_cols, names(detection_df))
  if (length(missing_cols) > 0) {
    stop("detection_df is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # ---- 0b. Clean coordinates -----------------------------------------------
  n_before     <- nrow(detection_df)
  detection_df <- detection_df %>%
    filter(
      !is.na(longitude), !is.na(latitude),
      longitude >= -180,  longitude <= 180,
      latitude  >=  -90,  latitude  <=  90
    )

  n_removed <- n_before - nrow(detection_df)
  if (n_removed > 0) {
    warning(n_removed, " row(s) removed: missing or out-of-range coordinates.\n",
            "  Check for NA or sentinel values (e.g. -9999, -1000) ",
            "in latitude/longitude columns.")
  }
  if (nrow(detection_df) == 0) {
    stop("No valid coordinates remain after filtering.")
  }

  # Print coordinate summary to help spot outliers
  message(sprintf(
    "Coordinate summary after cleaning (%d rows):\n  lon: [%.3f, %.3f]  lat: [%.3f, %.3f]",
    nrow(detection_df),
    min(detection_df$longitude), max(detection_df$longitude),
    min(detection_df$latitude),  max(detection_df$latitude)
  ))

  # ---- 0c. Coerce detection columns ----------------------------------------
  detection_df <- detection_df %>%
    mutate(
      be    = as.logical(be),
      be500 = as.logical(be500),
      det_category = case_when(
        isTRUE(be500)                   ~ "be500",    # yellow fill
        isTRUE(be) & !isTRUE(be500)    ~ "be_only",  # black fill
        TRUE                            ~ "none"      # open circle
      )
    )

  # ---- 1. Bounding box ------------------------------------------------------
  if (!is.null(xlim) && !is.null(ylim)) {
    # User-supplied extent
    lon_min <- xlim[1]; lon_max <- xlim[2]
    lat_min <- ylim[1]; lat_max <- ylim[2]
    message("Using user-supplied map extent.")
  } else {
    # Auto-fit with padding
    lon_min <- min(detection_df$longitude) - padding_deg
    lon_max <- max(detection_df$longitude) + padding_deg
    lat_min <- min(detection_df$latitude)  - padding_deg
    lat_max <- max(detection_df$latitude)  + padding_deg
    # Clamp to valid geographic bounds
    lon_min <- max(lon_min, -180); lon_max <- min(lon_max,  180)
    lat_min <- max(lat_min,  -90); lat_max <- min(lat_max,   90)
  }

  lon_range <- lon_max - lon_min
  lat_range <- lat_max - lat_min

  message(sprintf("Map extent: lon [%.2f, %.2f]  lat [%.2f, %.2f]  (%.1f x %.1f deg)",
                  lon_min, lon_max, lat_min, lat_max, lon_range, lat_range))

  # Warn if extent is very large — bathy download will be slow or may fail
  if (lon_range > 60 || lat_range > 40) {
    warning(
      sprintf("Map extent is large (%.0f x %.0f deg). ", lon_range, lat_range),
      "Consider supplying xlim/ylim to focus the map, or increase bathy_res ",
      "(e.g. bathy_res = 4) to reduce download size."
    )
  }

  # ---- 2. Bathymetry -------------------------------------------------------
  bathy_cache <- here("output", paste0(
    "bathy_cache_",
    paste(round(c(lon_min, lon_max, lat_min, lat_max), 1), collapse = "_"),
    "_res", bathy_res, ".rds"
  ))

  if (file.exists(bathy_cache)) {
    message("Loading cached bathymetry: ", bathy_cache)
    bathy_marmap <- readRDS(bathy_cache)
  } else {
    message(sprintf(
      "Downloading bathymetry (res = %g arc-min) for extent %.1f x %.1f deg...",
      bathy_res, lon_range, lat_range
    ))
    bathy_marmap <- tryCatch(
      getNOAA.bathy(
        lon1 = lon_min, lon2 = lon_max,
        lat1 = lat_min, lat2 = lat_max,
        resolution = bathy_res,
        keep = FALSE
      ),
      error = function(e) {
        stop(
          "Bathymetry download failed: ", conditionMessage(e), "\n",
          "  Possible causes:\n",
          "  1. Map extent is too large — try xlim/ylim to restrict the area, ",
          "or increase bathy_res (e.g. bathy_res = 4).\n",
          "  2. NOAA server is temporarily unavailable — try again later.\n",
          "  3. No internet connection."
        )
      }
    )

    # Validate the returned object before caching
    if (is.null(bathy_marmap) || length(bathy_marmap) == 0) {
      stop("Bathymetry download returned empty result. ",
           "Try a smaller extent or larger bathy_res value.")
    }

    if (keep_bathy) {
      dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
      saveRDS(bathy_marmap, bathy_cache)
      message("Bathymetry cached to: ", bathy_cache)
    }
  }

  # Convert to data frame
  bathy_df <- marmap::fortify.bathy(bathy_marmap) %>%
    rename(lon = x, lat = y, depth = z)

  # ---- 3. Land polygons ----------------------------------------------------
  land <- ne_countries(scale = "medium", returnclass = "sf")

  # ---- 4. Point size -------------------------------------------------------
  if (identical(point_size, "auto")) {
    pt_size <- .auto_point_size(lon_range, lat_range, nrow(detection_df))
    message("Auto point size: ", round(pt_size, 2))
  } else {
    pt_size <- as.numeric(point_size)
  }
  pt_stroke <- max(0.4, pt_size * 0.15)

  # ---- 5. Aesthetic scales -------------------------------------------------
  fill_values  <- c("be500" = "#FFD700", "be_only" = "black", "none" = NA)
  color_values <- c("be500" = "black",   "be_only" = "black", "none" = "black")
  shape_values <- c("be500" = 21,        "be_only" = 21,      "none" = 21)

  det_labels <- c(
    "be500"   = "be500 detected",
    "be_only" = "be detected (not be500)",
    "none"    = "No detection"
  )

  # ---- 6. Build map --------------------------------------------------------
  p <- ggplot() +

    # Bathymetry
    geom_raster(
      data        = bathy_df %>% filter(depth <= 0),
      mapping     = aes(x = lon, y = lat, fill = depth),
      interpolate = TRUE
    ) +
    scale_fill_gradientn(
      colours  = c("#1a3d5c", "#2e6fa8", "#5ba4cf", "#a8d0e6", "#d6eaf8"),
      values   = scales::rescale(c(-11000, -4000, -2000, -500, 0)),
      na.value = "#d9c9a3",
      name     = "Depth (m)",
      guide    = guide_colorbar(
        title.position = "top",
        barwidth       = unit(0.4, "cm"),
        barheight      = unit(3,   "cm")
      )
    ) +

    # Land
    geom_sf(data = land, fill = "#d9c9a3", color = "black", linewidth = 0.3) +

    # Detection points
    geom_point(
      data    = detection_df,
      mapping = aes(x = longitude, y = latitude,
                    fill = det_category, color = det_category,
                    shape = det_category),
      size   = pt_size,
      stroke = pt_stroke
    ) +
    scale_shape_manual(values = shape_values, labels = det_labels, name = "Detection") +
    scale_color_manual(values = color_values, labels = det_labels, name = "Detection") +
    scale_fill_manual( values = fill_values,  labels = det_labels, name = "Detection",
                       na.value = NA) +

    # Coordinate limits
    coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE) +

    # Axis labels in decimal degrees with cardinal suffix
    scale_x_continuous(
      breaks = pretty(c(lon_min, lon_max), n = 5),
      labels = function(x)
        paste0(abs(x), ifelse(x < 0, "\u00b0W", ifelse(x > 0, "\u00b0E", "\u00b0")))
    ) +
    scale_y_continuous(
      breaks = pretty(c(lat_min, lat_max), n = 5),
      labels = function(y)
        paste0(abs(y), ifelse(y < 0, "\u00b0S", ifelse(y > 0, "\u00b0N", "\u00b0")))
    ) +

    # Scale bar & north arrow
    ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height   = unit(1.2, "cm"),
      width    = unit(1.0, "cm"),
      style    = north_arrow_fancy_orienteering()
    ) +

    # Theme
    theme_bw(base_size = 12) +
    theme(
      axis.title        = element_blank(),
      panel.grid.major  = element_line(color = "white", linewidth = 0.3, linetype = "dashed"),
      panel.border      = element_rect(color = "black", linewidth = 0.8),
      legend.background = element_rect(fill = alpha("white", 0.8), color = "grey60"),
      legend.key        = element_blank(),
      legend.title      = element_text(face = "bold", size = 10),
      legend.text       = element_text(size = 9),
      legend.position   = "right",
      plot.margin       = margin(5, 5, 5, 5)
    ) +

    # Merge fill/color/shape into one legend entry
    guides(
      fill  = guide_legend(override.aes = list(
        fill   = c("#FFD700", "black", NA),
        color  = "black",
        shape  = 21,
        size   = 3,
        stroke = 0.6
      )),
      color = "none",
      shape = "none"
    )

  # ---- 7. Save -------------------------------------------------------------
  dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
  out_path <- here("output", paste0(output_name, ".png"))

  ggsave(
    filename = out_path,
    plot     = p,
    width    = fig_width,
    height   = fig_height,
    dpi      = dpi,
    units    = "in",
    bg       = "white"
  )

  message(sprintf("\n\u2713 Map saved: %s\n  %g x %g in  |  %d dpi",
                  out_path, fig_width, fig_height, dpi))

  invisible(p)
}


# ==============================================================
# Example usage
# ==============================================================
#
# library(here)
# detection_df <- readRDS(here("data", "brydes_df.rds"))
#
# # Auto-fit extent, default 1 arc-min bathy
# detection_map(detection_df, output_name = "brydes_detections")
#
# # Restrict to a specific area (recommended for large multi-survey datasets)
# detection_map(
#   detection_df,
#   output_name = "brydes_hiceas",
#   xlim        = c(-180, -130),
#   ylim        = c(15, 50)
# )
#
# # Fine bathy (15 arc-sec), custom point size, 600 dpi
# detection_map(
#   detection_df,
#   output_name = "brydes_hires",
#   xlim        = c(-165, -150),
#   ylim        = c(20, 35),
#   bathy_res   = 0.25,
#   point_size  = 2,
#   dpi         = 600
# )
