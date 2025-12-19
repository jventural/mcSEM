# =============================================================================
# INTERNAL EFA FUNCTIONS
# =============================================================================

#' Create EFA model syntax for lavaan
#' @noRd
.create_efa_syntax <- function(n_factors, n_items, item_prefix = "item") {
  item_names <- paste0(item_prefix, 1:n_items)
  items_str <- paste(item_names, collapse = " + ")


  factors <- paste0("efa(\"efa\")*f", 1:n_factors, collapse = " + ")
  model <- paste0(factors, " =~ ", items_str)

  model
}


#' Run a single EFA simulation replication
#' @noRd
.run_single_rep_efa <- function(i, n, sigma, n_factors, n_items, n_categories,
                                 estimator, rotation, seed, item_names = NULL) {
  result <- list(
    converged = FALSE,
    cfi = NA, tli = NA, rmsea = NA, srmr = NA
  )

  tryCatch({
    # Simulate data
    if (is.null(n_categories) || n_categories == 0) {
      sim_data <- .simulate_continuous_data(n, sigma, seed = seed + i)
    } else {
      sim_data <- .simulate_ordinal_data(n, sigma, n_categories, seed = seed + i)
    }

    # Rename columns if item_names provided
    if (!is.null(item_names)) {
      colnames(sim_data) <- item_names
    }

    # Create EFA model syntax
    model_syntax <- .create_efa_syntax(n_factors, n_items,
                                        item_prefix = gsub("[0-9]+$", "", colnames(sim_data)[1]))

    # Fit EFA model
    if (estimator == "WLSMV") {
      fit <- lavaan::cfa(model_syntax, data = sim_data,
                         ordered = TRUE, estimator = "WLSMV",
                         rotation = rotation, mimic = "Mplus")
    } else {
      fit <- lavaan::cfa(model_syntax, data = sim_data,
                         estimator = estimator, rotation = rotation)
    }

    # Check convergence
    if (lavaan::lavInspect(fit, "converged")) {
      result$converged <- TRUE

      # Get fit measures (use scaled versions for WLSMV)
      if (estimator == "WLSMV") {
        fm <- lavaan::fitmeasures(fit, c("cfi.scaled", "tli.scaled",
                                          "rmsea.scaled", "srmr"))
        result$cfi <- fm["cfi.scaled"]
        result$tli <- fm["tli.scaled"]
        result$rmsea <- fm["rmsea.scaled"]
        result$srmr <- fm["srmr"]
      } else {
        fm <- lavaan::fitmeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
        result$cfi <- fm["cfi"]
        result$tli <- fm["tli"]
        result$rmsea <- fm["rmsea"]
        result$srmr <- fm["srmr"]
      }
    }

  }, error = function(e) {
    # Keep defaults
  }, warning = function(w) {
    # Ignore warnings
  })

  result
}


#' Run EFA simulation for a specific sample size
#' @noRd
.simulate_for_n_efa <- function(n, reps, sigma, n_factors, n_items, n_categories,
                                 estimator, rotation, thresholds, parallel, n_cores,
                                 seed, item_names = NULL) {

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl,
                            c("n", "sigma", "n_factors", "n_items", "n_categories",
                              "estimator", "rotation", "seed", "item_names"),
                            envir = environment())

    # Export internal functions
    parallel::clusterExport(cl,
                            c(".run_single_rep_efa", ".simulate_ordinal_data",
                              ".simulate_continuous_data", ".create_efa_syntax"),
                            envir = asNamespace("mcSEM"))

    parallel::clusterEvalQ(cl, {
      library(MASS)
      library(lavaan)
    })

    results_list <- parallel::parLapply(cl, 1:reps, function(i) {
      .run_single_rep_efa(i, n, sigma, n_factors, n_items, n_categories,
                          estimator, rotation, seed, item_names)
    })

    parallel::stopCluster(cl)

  } else {
    results_list <- lapply(1:reps, function(i) {
      .run_single_rep_efa(i, n, sigma, n_factors, n_items, n_categories,
                          estimator, rotation, seed, item_names)
    })
  }

  # Extract results
  converged <- sapply(results_list, function(x) x$converged)
  cfi <- sapply(results_list, function(x) x$cfi)
  rmsea <- sapply(results_list, function(x) x$rmsea)
  srmr <- sapply(results_list, function(x) x$srmr)

  conv_idx <- which(converged)

  # Calculate summary statistics
  list(
    n = n,
    convergence_rate = mean(converged, na.rm = TRUE),
    n_converged = sum(converged, na.rm = TRUE),
    cfi_mean = if (length(conv_idx) > 0) mean(cfi[conv_idx], na.rm = TRUE) else NA,
    cfi_sd = if (length(conv_idx) > 0) stats::sd(cfi[conv_idx], na.rm = TRUE) else NA,
    cfi_q05 = if (length(conv_idx) > 0) stats::quantile(cfi[conv_idx], 0.05, na.rm = TRUE) else NA,
    rmsea_mean = if (length(conv_idx) > 0) mean(rmsea[conv_idx], na.rm = TRUE) else NA,
    rmsea_sd = if (length(conv_idx) > 0) stats::sd(rmsea[conv_idx], na.rm = TRUE) else NA,
    rmsea_q95 = if (length(conv_idx) > 0) stats::quantile(rmsea[conv_idx], 0.95, na.rm = TRUE) else NA,
    srmr_mean = if (length(conv_idx) > 0) mean(srmr[conv_idx], na.rm = TRUE) else NA,
    srmr_sd = if (length(conv_idx) > 0) stats::sd(srmr[conv_idx], na.rm = TRUE) else NA,
    srmr_q95 = if (length(conv_idx) > 0) stats::quantile(srmr[conv_idx], 0.95, na.rm = TRUE) else NA,
    prop_cfi_good = if (length(conv_idx) > 0) mean(cfi[conv_idx] >= thresholds$cfi, na.rm = TRUE) else NA,
    prop_rmsea_good = if (length(conv_idx) > 0) mean(rmsea[conv_idx] <= thresholds$rmsea, na.rm = TRUE) else NA,
    prop_srmr_good = if (length(conv_idx) > 0) mean(srmr[conv_idx] <= thresholds$srmr, na.rm = TRUE) else NA,
    prop_all_good = if (length(conv_idx) > 0) {
      mean(cfi[conv_idx] >= thresholds$cfi &
             rmsea[conv_idx] <= thresholds$rmsea &
             srmr[conv_idx] <= thresholds$srmr, na.rm = TRUE)
    } else NA
  )
}


#' Create EFA-specific full plot
#' @noRd
.create_full_plot_efa <- function(results_df, n_recommended, thresholds,
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
  main_title <- sprintf("Monte Carlo Sample Size Analysis for EFA\nRECOMMENDED SAMPLE SIZE: %d PARTICIPANTS", n_recommended)

  p_combined <- (p1 | p2) / (p3 | p4)

  p_final <- p_combined +
    patchwork::plot_annotation(
      title = main_title,
      subtitle = subtitle_extra,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 14, color = "red", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0.5)
      )
    )

  p_final
}
