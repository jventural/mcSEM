# =============================================================================
# INTERNAL PLOTTING FUNCTIONS
# =============================================================================

#' Create simple power plot
#' @noRd
.create_power_plot <- function(results_df, n_recommended, thresholds, subtitle = "") {

  df_long <- data.frame(
    n = rep(results_df$n, 3),
    Index = factor(rep(c("CFI", "RMSEA", "SRMR"), each = nrow(results_df)),
                   levels = c("CFI", "RMSEA", "SRMR")),
    Proportion = c(results_df$prop_cfi_good,
                   results_df$prop_rmsea_good,
                   results_df$prop_srmr_good)
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = n, y = Proportion, color = Index)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray40") +
    ggplot2::geom_vline(xintercept = n_recommended, linetype = "dotted",
                        color = "red", linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
    ggplot2::scale_color_manual(values = c("CFI" = "#2196F3",
                                           "RMSEA" = "#E91E63",
                                           "SRMR" = "#4CAF50")) +
    ggplot2::labs(
      title = "Monte Carlo Sample Size Analysis for CFA",
      subtitle = sprintf("Recommended N = %d %s", n_recommended, subtitle),
      x = "Sample Size (N)",
      y = "Proportion meeting criterion",
      color = "Fit Index"
    ) +
    ggplot2::annotate("text", x = n_recommended + 10, y = 0.10,
                      label = sprintf("N = %d", n_recommended),
                      color = "red", fontface = "bold", hjust = 0, size = 4) +
    ggplot2::annotate("text", x = max(results_df$n), y = 0.82,
                      label = "Power 80%", color = "gray40",
                      hjust = 1, vjust = -0.5, size = 3.5) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(color = "gray40"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  p
}


#' Create comprehensive 4-panel plot
#' @noRd
.create_full_plot <- function(results_df, n_recommended, thresholds,
                               title_extra = "", subtitle_extra = "") {

  # Colors
  col_cfi <- "#2196F3"
  col_rmsea <- "#E91E63"
  col_srmr <- "#4CAF50"
  col_conv <- "#9C27B0"

  # Panel 1: Convergence
  p1 <- ggplot2::ggplot(results_df, ggplot2::aes(x = n, y = convergence_rate)) +
    ggplot2::geom_line(color = col_conv, linewidth = 1.2) +
    ggplot2::geom_point(color = col_conv, size = 3) +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = n_recommended, linetype = "dotted",
                        color = "red", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                limits = c(max(0.5, min(results_df$convergence_rate) - 0.1), 1.02)) +
    ggplot2::labs(title = "Convergence Rate", x = "N", y = "Convergence") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

  # Panel 2: CFI
  p2 <- ggplot2::ggplot(results_df, ggplot2::aes(x = n)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = cfi_q05, ymax = cfi_mean),
                         fill = col_cfi, alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = cfi_mean), color = col_cfi, linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = cfi_mean), color = col_cfi, size = 2) +
    ggplot2::geom_hline(yintercept = thresholds$cfi, linetype = "dashed", color = "darkgreen") +
    ggplot2::geom_vline(xintercept = n_recommended, linetype = "dotted",
                        color = "red", linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(min(0.90, min(results_df$cfi_q05, na.rm = TRUE) - 0.02), 1.0)) +
    ggplot2::labs(title = sprintf("CFI (mean & 5th percentile) - Threshold: %.2f", thresholds$cfi),
                  x = "N", y = "CFI") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

  # Panel 3: RMSEA
  p3 <- ggplot2::ggplot(results_df, ggplot2::aes(x = n)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = rmsea_mean, ymax = rmsea_q95),
                         fill = col_rmsea, alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = rmsea_mean), color = col_rmsea, linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = rmsea_mean), color = col_rmsea, size = 2) +
    ggplot2::geom_hline(yintercept = thresholds$rmsea, linetype = "dashed", color = "darkgreen") +
    ggplot2::geom_vline(xintercept = n_recommended, linetype = "dotted",
                        color = "red", linewidth = 1) +
    ggplot2::scale_y_continuous(limits = c(0, max(0.10, max(results_df$rmsea_q95, na.rm = TRUE) + 0.01))) +
    ggplot2::labs(title = sprintf("RMSEA (mean & 95th percentile) - Threshold: %.3f", thresholds$rmsea),
                  x = "N", y = "RMSEA") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

  # Panel 4: Power
  df_power <- data.frame(
    n = rep(results_df$n, 3),
    Index = factor(rep(c("CFI", "RMSEA", "SRMR"), each = nrow(results_df)),
                   levels = c("CFI", "RMSEA", "SRMR")),
    Proportion = c(results_df$prop_cfi_good,
                   results_df$prop_rmsea_good,
                   results_df$prop_srmr_good)
  )

  p4 <- ggplot2::ggplot(df_power, ggplot2::aes(x = n, y = Proportion, color = Index)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = 0.80, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = n_recommended, linetype = "dotted",
                        color = "red", linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
    ggplot2::scale_color_manual(values = c("CFI" = col_cfi, "RMSEA" = col_rmsea, "SRMR" = col_srmr)) +
    ggplot2::labs(title = "Statistical Power (% meeting criterion)",
                  x = "N", y = "Proportion", color = "") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )

  # Combine panels
  main_title <- sprintf("Monte Carlo Sample Size Analysis for CFA\nRECOMMENDED SAMPLE SIZE: %d PARTICIPANTS", n_recommended)

  p_final <- (p1 | p2) / (p3 | p4) +
    patchwork::plot_annotation(
      title = main_title,
      subtitle = subtitle_extra
    ) &
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "red", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0.5)
    )

  p_final
}
