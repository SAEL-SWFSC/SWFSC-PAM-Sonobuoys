library(dplyr)
library(gt)
library(ggplot2)

analyze_burst_ipi <- function(data, maxIPI,
                              plot_histogram  = TRUE,
                              bin_width       = NULL,
                              show_median     = TRUE,
                              peak_method     = c("auto", "manual", "none"),
                              manual_peaks    = NULL,
                              table_path      = "ipi_summary_table.png",
                              plot_path       = "ipi_histogram.png") {
  
  peak_method <- match.arg(peak_method)
  
  # ── 1. Build sonobuoy_ID, sort, calculate IPI ─────────────────────────────
  processed_data <- data %>%
    mutate(sonobuoy_ID = paste(cruise, dat_number, sep = "-")) %>%
    arrange(sonobuoy_ID, date) %>%
    group_by(sonobuoy_ID) %>%
    mutate(IPI = lead(center_time_s) - center_time_s) %>%
    ungroup()
  
  # ── 2. Filter to valid IPIs (≤ maxIPI, non-NA) ────────────────────────────
  valid_ipi <- processed_data %>%
    filter(!is.na(IPI), IPI <= maxIPI) %>%
    pull(IPI)
   
  
  # ── 3. Summary table (data) ────────────────────────────────────────────────
  summary_df <- processed_data %>%
    filter(!is.na(IPI)) %>%
    mutate(IPI = round(IPI, digits= 1))%>%
    group_by(sonobuoy_ID) %>%
    summarise(
      exceeded_max_count = sum(IPI > maxIPI),
      mean_IPI           = round((mean(IPI[IPI <= maxIPI],   na.rm = TRUE)), 1),
      median_IPI         = round((median(IPI[IPI <= maxIPI], na.rm = TRUE)), 1),
      range_IPI = paste(
        sprintf("%.1f", min(IPI[IPI <= maxIPI], na.rm = TRUE)),
        sprintf("%.1f", max(IPI[IPI <= maxIPI], na.rm = TRUE)),
        sep = " - "
      ),
      .groups = "drop"
    ) %>%
    mutate(range_IPI = if_else(grepl("Inf|NaN", range_IPI), NA_character_, range_IPI))
  
  # ── 4. Publication-style gt table ──────────────────────────────────────────
  publication_table <- summary_df %>%
    gt() %>%
    cols_label(
      sonobuoy_ID         = "Sonobuoy ID",
      exceeded_max_count  = "Exceeded Max (n)",
      mean_IPI            = "Mean IPI (s)",
      median_IPI          = "Median IPI (s)",
      range_IPI           = "Range (s)"
    ) %>%
    fmt_number(columns = c(mean_IPI, median_IPI, range_IPI), decimals = 1) %>%
    tab_style(
      style     = cell_text(font = "Times New Roman"),
      locations = list(cells_body(), cells_column_labels(), cells_title())
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold", font = "Times New Roman"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_borders(
        sides  = "bottom",
        color  = "black",
        style  = "double",
        weight = px(3)
      ),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      table.width                    = pct(100),
      container.width                = pct(100),
      table.border.top.color         = "black",
      table.border.top.width         = px(2),
      table.border.bottom.color      = "black",
      table.border.bottom.width      = px(2),
      column_labels.border.top.color = "transparent",
      data_row.padding               = px(6),
      table.font.names               = "Times New Roman"
    ) %>%
    tab_style(
      style     = cell_text(whitespace = "nowrap"),
      locations = list(cells_body(), cells_column_labels())
    )
  
  gtsave(publication_table, table_path)
  
  # ── 5. Histogram ────────────────────────────────────────────────────────
  p <- NULL
  
  if (plot_histogram && length(valid_ipi) > 0) {
    
    # Bin width: user-supplied or Freedman-Diaconis
    bw <- if (!is.null(bin_width)) {
      bin_width
    } else {
      bw_fd <- 2 * IQR(valid_ipi) / (length(valid_ipi)^(1/3))
      if (bw_fd == 0) diff(range(valid_ipi)) / 30 else bw_fd
    }
    
    overall_median <- median(valid_ipi)
    
    # Base plot
    p <- ggplot(data.frame(IPI = valid_ipi), aes(x = IPI)) +
      geom_histogram(binwidth  = bw,
                     fill      = "grey75",
                     color     = "black",
                     linewidth = 0.3) +
      labs(
        title    = "Inter-Pulse Interval (IPI) Distribution",
        subtitle = paste0("All sonobuoys combined  |  IPI \u2264 ", maxIPI, " s  |  n = ",
                          length(valid_ipi)),
        x        = "IPI (s)",
        y        = "Count"
      ) +
      theme_classic(base_size = 13, base_family = "Times New Roman") +
      theme(
        plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey30", size = 11),
        axis.title    = element_text(face = "bold"),
        axis.text     = element_text(color = "black"),
        axis.line     = element_line(color = "black", linewidth = 0.4),
        panel.grid    = element_blank()
      )
    
    # Median line (optional)
    if (show_median) {
      p <- p +
        geom_vline(xintercept = overall_median,
                   color      = "black",
                   linewidth  = 0.8,
                   linetype   = "dashed") +
        annotate("text",
                 x      = overall_median,
                 y      = Inf,
                 label  = paste0("Median = ", round(overall_median, 3), " s"),
                 hjust  = -0.08,
                 vjust  = 1.5,
                 color  = "black",
                 family = "Times New Roman",
                 size   = 4)
    }
    
    # Automatic peak detection
    if (peak_method == "auto") {
      dens     <- density(valid_ipi, bw = "SJ")
      dens_df  <- data.frame(x = dens$x, y = dens$y)
      n        <- nrow(dens_df)
      is_peak  <- c(FALSE,
                    dens_df$y[2:(n-1)] > dens_df$y[1:(n-2)] &
                      dens_df$y[2:(n-1)] > dens_df$y[3:n],
                    FALSE)
      peaks    <- dens_df[is_peak, ]
      peaks    <- peaks[peaks$y >= 0.10 * max(peaks$y), ]
      
      hist_max   <- max(table(cut(valid_ipi,
                                  breaks = seq(min(valid_ipi), max(valid_ipi) + bw, by = bw))))
      dens_scale <- hist_max / max(dens$y)
      
      p <- p +
        geom_line(data  = data.frame(x = dens$x, y = dens$y * dens_scale),
                  aes(x = x, y = y),
                  color = "grey30", linewidth = 0.8, inherit.aes = FALSE)
      
      if (nrow(peaks) > 0) {
        peaks$y_scaled <- peaks$y * dens_scale
        p <- p +
          geom_vline(data     = peaks,
                     aes(xintercept = x),
                     color    = "black",
                     linetype = "dotdash",
                     linewidth = 0.7,
                     inherit.aes = FALSE) +
          geom_label(data = peaks,
                     aes(x     = x,
                         y     = y_scaled,
                         label = paste0("Peak\n", round(x, 3), " s")),
                     color    = "black",
                     fill     = "white",
                     family   = "Times New Roman",
                     size     = 3.3,
                     nudge_x  = diff(range(valid_ipi)) * 0.02,
                     inherit.aes = FALSE)
        
        message(length(peaks$x), " auto-detected peak(s) at IPI = ",
                paste(round(peaks$x, 4), collapse = ", "), " s")
      } else {
        message("No prominent peaks detected automatically.")
      }
    }
    
    # Manual peak annotation
    if (peak_method == "manual" && !is.null(manual_peaks)) {
      manual_df <- data.frame(x = manual_peaks)
      p <- p +
        geom_vline(data      = manual_df,
                   aes(xintercept = x),
                   color     = "black",
                   linetype  = "dotdash",
                   linewidth = 0.7,
                   inherit.aes = FALSE) +
        geom_label(data = manual_df,
                   aes(x     = x,
                       y     = Inf,
                       label = paste0("Peak\n", round(x, 3), " s")),
                   color   = "black",
                   fill    = "white",
                   family  = "Times New Roman",
                   size    = 3.3,
                   vjust   = 1.5,
                   inherit.aes = FALSE)
    }
    
    ggsave(plot_path, plot = p, width = 8, height = 5, dpi = 300)
  }
  
  # ── 6. Attach outputs and return ───────────────────────────────────────────
  attr(summary_df, "histogram") <- p
  attr(summary_df, "gt_table")  <- publication_table
  
  return(summary_df)
}