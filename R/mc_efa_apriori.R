#' Monte Carlo Sample Size Estimation for EFA (A Priori)
#'
#' Estimates optimal sample size for Exploratory Factor Analysis using
#' Monte Carlo simulation WITHOUT requiring existing data. Uses theoretical
#' model parameters to simulate data.
#'
#' @param n_factors Number of latent factors expected
#' @param n_items Total number of items in the scale
#' @param items_per_factor Number of items per factor. Can be a single number
#'   (same for all factors) or a vector (different for each factor)
#' @param loadings Expected factor loadings. Single value for average loading,
#'   or vector of length equal to total items (default: 0.70)
#' @param factor_cors Expected correlation between factors. Single value for
#'   uniform correlation, or a matrix (default: 0.30)
#' @param n_categories Number of response categories for Likert scale.
#'   Use NULL for continuous data (default: 5)
#' @param cross_loadings Magnitude of cross-loadings (default: 0.10)
#' @param n_range Numeric vector of sample sizes to evaluate (default: seq(100, 600, by = 50))
#' @param reps Number of Monte Carlo replications per sample size (default: 500)
#' @param criterion Fit criterion: "strict", "moderate", or "flexible" (default: "moderate")
#' @param power_target Target statistical power (default: 0.80)
#' @param estimator Estimator: "WLSMV" for ordinal, "ML" for continuous (default: "WLSMV")
#' @param rotation Rotation method: "oblimin", "varimax", "promax", etc. (default: "oblimin")
#' @param parallel Use parallel processing (default: TRUE)
#' @param n_cores Number of cores (default: detectCores() - 1)
#' @param seed Random seed (default: 12345)
#' @param verbose Print progress (default: TRUE)
#'
#' @return An object of class "mcSEM" containing:
#' \itemize{
#'   \item n_recommended: Recommended sample size
#'   \item results: Data frame with results for each sample size
#'   \item details: List with sample size for each criterion
#'   \item model: Theoretical model parameters
#'   \item config: Configuration parameters used
#'   \item plot: ggplot2 object (4-panel plot)
#' }
#'
#' @examples
#' \dontrun{
#' # Simple example: 4 factors, 20 items total, Likert 5-point scale
#' result <- mc_efa_apriori(
#'   n_factors = 4,
#'   n_items = 20,
#'   items_per_factor = 5,
#'   loadings = 0.70,
#'   n_categories = 5
#' )
#'
#' # View results
#' print(result)
#'
#' # Save plot
#' ggsave("efa_apriori_plot.png", result$plot, width = 12, height = 9)
#'
#' # Different items per factor
#' result2 <- mc_efa_apriori(
#'   n_factors = 3,
#'   n_items = 15,
#'   items_per_factor = c(6, 5, 4),
#'   loadings = 0.65,
#'   rotation = "varimax"
#' )
#' }
#'
#' @export
mc_efa_apriori <- function(n_factors,
                            n_items,
                            items_per_factor = NULL,
                            loadings = 0.70,
                            factor_cors = 0.30,
                            n_categories = 5,
                            cross_loadings = 0.10,
                            n_range = seq(100, 600, by = 50),
                            reps = 500,
                            criterion = "moderate",
                            power_target = 0.80,
                            estimator = "WLSMV",
                            rotation = "oblimin",
                            parallel = TRUE,
                            n_cores = NULL,
                            seed = 12345,
                            verbose = TRUE) {

  set.seed(seed)

  # Setup parallel
  if (parallel && is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Process items_per_factor
 if (is.null(items_per_factor)) {
    items_per_factor <- rep(floor(n_items / n_factors), n_factors)
    # Distribute remainder
    remainder <- n_items - sum(items_per_factor)
    if (remainder > 0) {
      items_per_factor[1:remainder] <- items_per_factor[1:remainder] + 1
    }
  } else if (length(items_per_factor) == 1) {
    items_per_factor <- rep(items_per_factor, n_factors)
  }

  # Validate
  if (sum(items_per_factor) != n_items) {
    stop("Sum of items_per_factor must equal n_items")
  }

  # Get thresholds
  thresholds <- .get_thresholds(criterion)

  # Create theoretical model components for population
  loading_matrix <- .create_loading_matrix(n_factors, items_per_factor, loadings,
                                            cross_loading_mag = cross_loadings)
  phi_matrix <- .create_phi_matrix(n_factors, factor_cors)
  residual_matrix <- .create_residual_matrix(n_items, residual_cor_mag = 0, prop_correlated = 0)
  sigma_pop <- .generate_sigma(loading_matrix, phi_matrix, residual_matrix)

  if (verbose) {
    cat("\n")
    cat("================================================================\n")
    cat("   MONTE CARLO SAMPLE SIZE ANALYSIS FOR EFA (A PRIORI)\n")
    cat("================================================================\n\n")
    cat("=== THEORETICAL MODEL ===\n\n")
    cat(sprintf("  Factors: %d\n", n_factors))
    cat(sprintf("  Items per factor: %s\n", paste(items_per_factor, collapse = ", ")))
    cat(sprintf("  Total items: %d\n", n_items))
    cat(sprintf("  Factor loadings: %.2f (average)\n", mean(loading_matrix[loading_matrix > 0.3])))
    cat(sprintf("  Factor correlations: %.2f (average)\n", mean(phi_matrix[lower.tri(phi_matrix)])))
    cat(sprintf("  Cross-loadings: %.2f\n", cross_loadings))
    cat(sprintf("  Response categories: %s\n", ifelse(is.null(n_categories), "Continuous", as.character(n_categories))))
    cat(sprintf("  Estimator: %s\n", estimator))
    cat(sprintf("  Rotation: %s\n\n", rotation))

    cat("=== FIT CRITERIA ===\n\n")
    cat(sprintf("  Criterion: %s\n", criterion))
    cat(sprintf("  CFI >= %.2f\n", thresholds$cfi))
    cat(sprintf("  RMSEA <= %.3f\n", thresholds$rmsea))
    cat(sprintf("  SRMR <= %.3f\n", thresholds$srmr))
    cat(sprintf("  Power target: %.0f%%\n\n", power_target * 100))

    cat("=== SIMULATION SETTINGS ===\n\n")
    cat(sprintf("  Replications: %d per sample size\n", reps))
    cat(sprintf("  Sample sizes: %d to %d\n", min(n_range), max(n_range)))
    if (parallel) cat(sprintf("  Parallel: %d cores\n", n_cores))
    cat("\n")

    cat("================================================================\n")
    cat("   RUNNING MONTE CARLO SIMULATION\n")
    cat("================================================================\n\n")
  }

  # Run simulation for each sample size
  all_results <- list()

  for (n in n_range) {
    if (verbose) cat(sprintf("  N = %3d ", n))

    result_n <- .simulate_for_n_efa(
      n = n,
      reps = reps,
      sigma = sigma_pop,
      n_factors = n_factors,
      n_items = n_items,
      n_categories = n_categories,
      estimator = estimator,
      rotation = rotation,
      thresholds = thresholds,
      parallel = parallel,
      n_cores = n_cores,
      seed = seed + n,
      item_names = NULL
    )

    all_results[[as.character(n)]] <- result_n

    if (verbose) {
      cat(sprintf("| Conv: %3.0f%% | CFI: %.3f [%3.0f%%] | RMSEA: %.3f [%3.0f%%] | SRMR: %.3f [%3.0f%%]\n",
                  result_n$convergence_rate * 100,
                  result_n$cfi_mean, result_n$prop_cfi_good * 100,
                  result_n$rmsea_mean, result_n$prop_rmsea_good * 100,
                  result_n$srmr_mean, result_n$prop_srmr_good * 100))
    }
  }

  # Create results data frame
  results_df <- data.frame(
    n = n_range,
    convergence_rate = sapply(all_results, function(x) x$convergence_rate),
    cfi_mean = sapply(all_results, function(x) x$cfi_mean),
    cfi_q05 = sapply(all_results, function(x) x$cfi_q05),
    rmsea_mean = sapply(all_results, function(x) x$rmsea_mean),
    rmsea_q95 = sapply(all_results, function(x) x$rmsea_q95),
    srmr_mean = sapply(all_results, function(x) x$srmr_mean),
    srmr_q95 = sapply(all_results, function(x) x$srmr_q95),
    prop_cfi_good = sapply(all_results, function(x) x$prop_cfi_good),
    prop_rmsea_good = sapply(all_results, function(x) x$prop_rmsea_good),
    prop_srmr_good = sapply(all_results, function(x) x$prop_srmr_good),
    prop_all_good = sapply(all_results, function(x) x$prop_all_good)
  )
  rownames(results_df) <- NULL

  # Determine recommended N
  n_convergence <- results_df$n[which(results_df$convergence_rate >= 0.95)[1]]
  n_cfi <- results_df$n[which(results_df$prop_cfi_good >= power_target)[1]]
  n_rmsea <- results_df$n[which(results_df$prop_rmsea_good >= power_target)[1]]
  n_srmr <- results_df$n[which(results_df$prop_srmr_good >= power_target)[1]]
  n_all <- results_df$n[which(results_df$prop_all_good >= power_target)[1]]

  n_values <- c(n_convergence, n_cfi, n_rmsea, n_srmr)
  n_values <- n_values[!is.na(n_values)]

  n_recommended <- if (length(n_values) > 0) max(n_values) else max(n_range)

  # Create plot
  subtitle <- sprintf("Rotation: %s | Criterion: %s | %d replications",
                      rotation, criterion, reps)
  plot_obj <- .create_full_plot_efa(results_df, n_recommended, thresholds,
                                     subtitle_extra = subtitle)

  if (verbose) {
    cat("\n")
    cat("================================================================\n")
    cat("   RECOMMENDATION\n")
    cat("================================================================\n\n")
    cat(sprintf("  Minimum N for convergence (>= 95%%): %s\n",
                ifelse(is.na(n_convergence), "Not achieved", as.character(n_convergence))))
    cat(sprintf("  Minimum N for CFI >= %.2f (80%% power): %s\n", thresholds$cfi,
                ifelse(is.na(n_cfi), "Not achieved", as.character(n_cfi))))
    cat(sprintf("  Minimum N for RMSEA <= %.3f (80%% power): %s\n", thresholds$rmsea,
                ifelse(is.na(n_rmsea), "Not achieved", as.character(n_rmsea))))
    cat(sprintf("  Minimum N for SRMR <= %.3f (80%% power): %s\n", thresholds$srmr,
                ifelse(is.na(n_srmr), "Not achieved", as.character(n_srmr))))
    cat("\n")
    cat(sprintf("  >>> RECOMMENDED SAMPLE SIZE: %d PARTICIPANTS <<<\n\n", n_recommended))
  }

  # Create result object
  result <- list(
    n_recommended = n_recommended,
    results = results_df,
    details = list(
      n_convergence = n_convergence,
      n_cfi = n_cfi,
      n_rmsea = n_rmsea,
      n_srmr = n_srmr,
      n_all = n_all
    ),
    model = list(
      n_factors = n_factors,
      items_per_factor = items_per_factor,
      n_items = n_items,
      loadings = loading_matrix,
      phi = phi_matrix,
      sigma = sigma_pop,
      rotation = rotation
    ),
    config = list(
      type = "efa_a_priori",
      analysis = "EFA",
      cross_loadings = cross_loadings,
      n_categories = n_categories,
      criterion = criterion,
      thresholds = thresholds,
      estimator = estimator,
      rotation = rotation,
      reps = reps,
      power_target = power_target
    ),
    plot = plot_obj
  )

  class(result) <- c("mcSEM", "list")

  result
}
