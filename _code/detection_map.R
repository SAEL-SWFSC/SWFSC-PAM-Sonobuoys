# ============================================================
# detection_map.R
# Publication-quality detection map with:
#   - GEBCO/NOAA bathymetry (light blues) + tan land
#   - Multiple species, each with a 'detected' and 'sighting' column
#   - Shape: triangle (sighting == TRUE) vs circle (sighting == FALSE)
#   - Fill: per-species colour (detected) or open/NA (not detected)
#   - Fully customisable legend labels and colours
# ============================================================
#
# install.packages(c("tidyverse", "here", "sf", "marmap",
#                    "ggspatial", "rnaturalearth", "rnaturalearthdata", "scales"))

library(tidyverse)
library(here)
library(sf)
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
  diag_deg       <- sqrt(lon_range^2 + lat_range^2)
  base           <- diag_deg * 0.08
  density_shrink <- max(0.4, 1 - log10(max(n_points, 1)) * 0.18)
  size           <- clamp(base * density_shrink, 1.2, 6)
  return(size)
}
clamp <- function(x, lo, hi) max(lo, min(hi, x))

# ----------------------------------------------------------
# Default colour palette (ColorBrewer Set1, colourblind-friendlier)
# ----------------------------------------------------------
.default_palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00",
  "#984EA3", "#A65628", "#F781BF", "#999999"
)

# ============================================================
# Main function
# ============================================================
#
# REQUIRED ARGUMENTS
#   detection_df  : data frame with at minimum latitude, longitude, plus the
#                   columns named in each species entry below.
#   output_name   : filename stem written to here("output/<name>.png")
#   species       : named list — one entry per species to map.
#                   Each entry is itself a list with:
#                     detected  : column name (logical or numeric); TRUE/1 = detected
#                     sighting  : column name (logical/0-1); TRUE = visual sighting
#                     threshold : (optional) for numeric 'detected' columns, the
#                                 value above which a row counts as detected
#                                 (default 0, so any value > 0 = detected)
#                     color     : (optional) fill colour string; auto-assigned if omitted
#                     label     : (optional) species label for the legend;
#                                 defaults to the species name
#
# OPTIONAL ARGUMENTS
#   point_size        : numeric, or "auto" (default)
#   padding_deg       : degrees of padding around auto extent (default 1)
#   xlim / ylim       : c(min, max) to override auto extent
#   label_sighting    : legend suffix when sighting == TRUE  (default "w/ sighting")
#   label_no_sighting : legend suffix when sighting == FALSE (default "acoustic only")
#   label_not_detected: legend label when detected == FALSE  (default "Not detected")
#   show_not_detected : include not-detected rows in the legend (default TRUE)
#   legend_title      : heading for the detection legend (default "Detection")
#   dpi               : output DPI (default 300)
#   fig_width / fig_height : figure dimensions in inches (default 10 x 8)
#   bathy_res         : arc-minutes (default 1; use 4 for fast draft)
#   keep_bathy        : cache downloaded bathy as RDS (default TRUE)
#
# EXAMPLE — see bottom of file
#
detection_map <- function(
    detection_df,
    output_name,
    species,                              # named list — see above
    point_size         = "auto",
    padding_deg        = 1,
    xlim               = NULL,
    ylim               = NULL,
    label_sighting     = "w/ sighting",
    label_no_sighting  = "acoustic only",
    label_not_detected = "Not detected",
    show_not_detected  = TRUE,
    legend_title       = "Detection",
    dpi                = 300,
    fig_width          = 10,
    fig_height         = 8,
    bathy_res          = 1,
    keep_bathy         = TRUE
) {

  # ---- 0. Validate species list --------------------------------------------
  if (!is.list(species) || is.null(names(species)) || any(names(species) == ""))
    stop("`species` must be a named list (one entry per species).")

  for (sp in names(species)) {
    entry <- species[[sp]]
    if (is.null(entry$detected))
      stop("species[['", sp, "']] is missing the 'detected' column name.")
    if (is.null(entry$sighting))
      stop("species[['", sp, "']] is missing the 'sighting' column name.")
    for (col in c(entry$detected, entry$sighting)) {
      if (!col %in% names(detection_df))
        stop("Column '", col, "' (species '", sp, "') not found in detection_df.")
    }
  }

  # Assign colours — use entry$color if supplied, else draw from palette
  pal     <- .default_palette
  pal_idx <- 1L
  for (sp in names(species)) {
    if (is.null(species[[sp]]$color)) {
      species[[sp]]$color <- pal[[pal_idx]]
      pal_idx <- pal_idx + 1L
      if (pal_idx > length(pal)) pal_idx <- 1L
    }
    # Default label = species name
    if (is.null(species[[sp]]$label))
      species[[sp]]$label <- sp
  }

  # ---- 0b. Validate required base columns ----------------------------------
  base_cols <- c("latitude", "longitude")
  missing   <- setdiff(base_cols, names(detection_df))
  if (length(missing) > 0)
    stop("detection_df is missing columns: ", paste(missing, collapse = ", "))

  # ---- 0c. Clean coordinates -----------------------------------------------
  n_before     <- nrow(detection_df)
  detection_df <- detection_df %>%
    mutate(
      longitude = suppressWarnings(as.numeric(longitude)),
      latitude  = suppressWarnings(as.numeric(latitude))
    ) %>%
    filter(
      !is.na(longitude), !is.na(latitude),
      longitude >= -180, longitude <= 180,
      latitude  >=  -90, latitude  <=  90
    )
  n_removed <- n_before - nrow(detection_df)
  if (n_removed > 0)
    warning(n_removed, " row(s) removed: missing or out-of-range coordinates.")
  if (nrow(detection_df) == 0)
    stop("No valid coordinates remain after filtering.")

  message(sprintf(
    "Coordinate summary (%d rows): lon [%.2f, %.2f]  lat [%.2f, %.2f]",
    nrow(detection_df),
    min(detection_df$longitude), max(detection_df$longitude),
    min(detection_df$latitude),  max(detection_df$latitude)
  ))

  # ---- 1. Bounding box -----------------------------------------------------
  if (!is.null(xlim) && !is.null(ylim)) {
    lon_min <- xlim[1]; lon_max <- xlim[2]
    lat_min <- ylim[1]; lat_max <- ylim[2]
    message("Using user-supplied map extent.")
  } else {
    lon_min <- max(min(detection_df$longitude) - padding_deg, -180)
    lon_max <- min(max(detection_df$longitude) + padding_deg,  180)
    lat_min <- max(min(detection_df$latitude)  - padding_deg,  -90)
    lat_max <- min(max(detection_df$latitude)  + padding_deg,   90)
  }
  lon_range <- lon_max - lon_min
  lat_range <- lat_max - lat_min
  message(sprintf("Map extent: lon [%.2f, %.2f]  lat [%.2f, %.2f]  (%.1f x %.1f deg)",
                  lon_min, lon_max, lat_min, lat_max, lon_range, lat_range))
  if (lon_range > 60 || lat_range > 40)
    warning(sprintf("Large extent (%.0f x %.0f deg). ", lon_range, lat_range),
            "Consider xlim/ylim or increase bathy_res (e.g. 4).")

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
    message(sprintf("Downloading bathymetry (res = %g arc-min)...", bathy_res))
    bathy_marmap <- tryCatch(
      getNOAA.bathy(lon1 = lon_min, lon2 = lon_max,
                    lat1 = lat_min, lat2 = lat_max,
                    resolution = bathy_res, keep = FALSE),
      error = function(e) stop(
        "Bathymetry download failed: ", conditionMessage(e), "\n",
        "  Try: smaller extent, larger bathy_res, or check internet."
      )
    )
    if (is.null(bathy_marmap) || length(bathy_marmap) == 0)
      stop("Bathymetry download returned empty result.")
    if (keep_bathy) {
      dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
      saveRDS(bathy_marmap, bathy_cache)
      message("Cached to: ", bathy_cache)
    }
  }
  bathy_df <- marmap::fortify.bathy(bathy_marmap) %>%
    rename(lon = x, lat = y, depth = z)

  # ---- 3. Land (cropped to extent) ----------------------------------------
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = lon_min, xmax = lon_max, ymin = lat_min, ymax = lat_max),
    crs = sf::st_crs(4326)
  ))
  land <- sf::st_crop(ne_countries(scale = "medium", returnclass = "sf"), bbox_sfc)

  # ---- 4. Point size -------------------------------------------------------
  if (identical(point_size, "auto")) {
    pt_size <- .auto_point_size(lon_range, lat_range, nrow(detection_df))
    message("Auto point size: ", round(pt_size, 2))
  } else {
    pt_size <- as.numeric(point_size)
  }
  pt_stroke <- max(0.4, pt_size * 0.15)

  # ---- 5. Build rendering table --------------------------------------------
  # Each row = one species × detection × sighting combination.
  # We expand all combinations per species, then look up the data rows.

  # Legend key definition (one row per visible legend entry)
  # Columns: grp_id, label, fill_color, shape, sp_name
  legend_rows <- list()
  # Rendering layers (list of geom_point calls)
  render_layers <- list()

  for (sp in names(species)) {
    entry    <- species[[sp]]
    sp_color <- entry$color
    sp_label <- entry$label
    det_col  <- entry$detected
    sig_col  <- entry$sighting

    # Classify rows for this species
    # threshold: if supplied, detected = (value > threshold); default 0 for
    # numeric columns (any value > 0 = TRUE), ignored for logical columns
    det_thresh <- if (!is.null(entry$threshold)) entry$threshold else 0

    sp_df <- detection_df %>%
      mutate(
        .detected = {
          raw <- .data[[det_col]]
          if (is.logical(raw)) raw
          else if (is.numeric(raw)) raw > det_thresh
          else as.logical(raw)
        },
        .sighting = as.logical(.data[[sig_col]]),
        .detected = replace_na(.detected, FALSE),
        .sighting = replace_na(.sighting, FALSE)
      )

    # Four possible sub-groups
    sub_groups <- list(
      list(id      = paste0(sp, ".det.sight"),
           label   = paste0(sp_label, " — detected, ", label_sighting),
           fill    = sp_color,
           shape   = 24L,     # triangle = sighting
           filter  = quote(.detected == TRUE  & .sighting == TRUE)),
      list(id      = paste0(sp, ".det.nosight"),
           label   = paste0(sp_label, " — detected, ", label_no_sighting),
           fill    = sp_color,
           shape   = 21L,     # circle = no sighting
           filter  = quote(.detected == TRUE  & .sighting == FALSE)),
      list(id      = paste0(sp, ".nodet.sight"),
           label   = paste0(sp_label, " — ", label_not_detected, ", ", label_sighting),
           fill    = NA_character_,
           shape   = 24L,
           filter  = quote(.detected == FALSE & .sighting == TRUE)),
      list(id      = paste0(sp, ".nodet.nosight"),
           label   = paste0(sp_label, " — ", label_not_detected),
           fill    = NA_character_,
           shape   = 21L,
           filter  = quote(.detected == FALSE & .sighting == FALSE))
    )

    for (sg in sub_groups) {
      # Skip "not detected" entries from legend if show_not_detected = FALSE
      is_not_detected <- grepl("nodet", sg$id)
      if (is_not_detected && !show_not_detected) next

      rows <- sp_df %>% filter(!!sg$filter)
      if (nrow(rows) == 0) next   # skip empty groups entirely

      # Rendering layer
      render_layers[[sg$id]] <- geom_point(
        data   = rows,
        aes(x = longitude, y = latitude),
        shape  = sg$shape,
        fill   = sg$fill,
        color  = "black",
        size   = pt_size,
        stroke = pt_stroke,
        na.rm  = TRUE
      )

      # Legend entry
      legend_rows[[sg$id]] <- tibble(
        grp_id  = sg$id,
        label   = sg$label,
        fill    = sg$fill,
        shape   = sg$shape
      )
    }
  }

  # Assemble legend data frame (order: detected-sighting, detected-nosighting,
  # nodet-sighting, nodet-nosighting, within each species in species order)
  legend_df <- bind_rows(legend_rows) %>%
    mutate(grp_id = factor(grp_id, levels = grp_id))  # preserve order

  # Dummy data frame with one row per legend entry (for color scale)
  dummy_df <- legend_df %>%
    mutate(longitude = mean(detection_df$longitude),
           latitude  = mean(detection_df$latitude))

  # ---- 6. Build map --------------------------------------------------------
  p <- ggplot() +

    # Bathymetry (the only fill scale)
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
      guide    = guide_colorbar(title.position = "top",
                                barwidth  = unit(0.4, "cm"),
                                barheight = unit(3,   "cm"))
    ) +

    # Land
    geom_sf(data = land, fill = "#d9c9a3", color = "black", linewidth = 0.3) +

    # Detection point layers (hardcoded aesthetics — no scale conflict)
    render_layers +

    # Invisible dummy points to drive the discrete color legend
    geom_point(
      data    = dummy_df,
      mapping = aes(x = longitude, y = latitude, color = grp_id),
      shape   = NA_integer_,
      size    = 0
    ) +
    scale_color_manual(
      values = setNames(rep("black", nrow(legend_df)), legend_df$grp_id),
      labels = setNames(legend_df$label,               legend_df$grp_id),
      name   = legend_title,
      drop   = FALSE,
      guide  = guide_legend(
        override.aes = list(
          shape  = legend_df$shape,
          fill   = legend_df$fill,
          color  = "black",
          size   = pt_size * 0.9,
          stroke = pt_stroke
        )
      )
    ) +

    # Coordinate limits
    coord_sf(xlim   = c(lon_min, lon_max),
             ylim   = c(lat_min, lat_max),
             expand = FALSE,
             crs    = sf::st_crs(4326)) +

    # Axis labels (decimal degrees with cardinal suffix)
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

    ggspatial::annotation_scale(
      location = "bl", width_hint = 0.25, text_cex = 0.7) +
    ggspatial::annotation_north_arrow(
      location = "br",
      height   = unit(1.2, "cm"), width = unit(1.0, "cm"),
      style    = north_arrow_fancy_orienteering()
    ) +

    theme_bw(base_size = 12) +
    theme(
      axis.title        = element_blank(),
      panel.grid.major  = element_line(color = "white", linewidth = 0.3,
                                       linetype = "dashed"),
      panel.border      = element_rect(color = "black", linewidth = 0.8),
      legend.background = element_rect(fill = alpha("white", 0.8), color = "grey60"),
      legend.key        = element_blank(),
      legend.title      = element_text(face = "bold", size = 10),
      legend.text       = element_text(size = 9),
      legend.position   = "right",
      plot.margin       = margin(5, 5, 5, 5)
    )

  # ---- 7. Save -------------------------------------------------------------
  dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
  out_path <- here("output", paste0(output_name, ".png"))
  ggsave(filename = out_path, plot = p,
         width = fig_width, height = fig_height,
         dpi = dpi, units = "in", bg = "white")
  message(sprintf("\n\u2713 Map saved: %s  (%g x %g in, %d dpi)",
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
# # Single species — minimal call
# detection_map(
#   detection_df,
#   output_name = "brydes_only",
#   species = list(
#     Brydes = list(detected = "be500", sighting = "brydes_sighting")
#   )
# )
#
# # Two species — auto colours
# detection_map(
#   detection_df,
#   output_name = "brydes_and_blue",
#   species = list(
#     Brydes = list(detected = "be500",    sighting = "brydes_sighting"),
#     Blue   = list(detected = "blue_det", sighting = "blue_sighting")
#   )
# )
#
# # Two species — custom colours, labels, restricted extent
# detection_map(
#   detection_df,
#   output_name = "survey_detections",
#   species = list(
#     Brydes = list(detected  = "be500",
#                  sighting   = "brydes_sighting",
#                  color      = "#E63946",
#                  label      = "Bryde's whale"),
#     Blue   = list(detected  = "blue_det",
#                  sighting   = "blue_sighting",
#                  color      = "#457B9D",
#                  label      = "Blue whale")
#   ),
#   xlim               = c(-180, -130),
#   ylim               = c(15,    50),
#   label_sighting     = "w/ visual",
#   label_no_sighting  = "acoustic only",
#   label_not_detected = "Not detected",
#   show_not_detected  = FALSE,   # hide not-detected rows from legend
#   legend_title       = "Species detections",
#   bathy_res          = 4        # coarse for fast draft
# )
