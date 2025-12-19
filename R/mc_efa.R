#' Monte Carlo Sample Size Estimation for EFA (A Posteriori)
#'
#' Estimates optimal sample size for Exploratory Factor Analysis using
#' Monte Carlo simulation with existing data. Evaluates power based on
#' CFI, RMSEA, and SRMR fit indices.
#'
#' @param data A data frame containing the items
#' @param n_factors Number of factors to extract
#' @param items A character vector of item names, or NULL to use all columns (default: NULL)
#' @param n_range Numeric vector of sample sizes to evaluate (default: seq(100, 500, by = 50))
#' @param reps Number of Monte Carlo replications per sample size (default: 500)
#' @param criterion Fit criterion: "strict", "moderate", or "flexible" (default: "moderate")
#' @param power_target Target statistical power (default: 0.80)
#' @param cor_type Correlation type: "pearson" or "polychoric" (default: "polychoric")
#' @param estimator Estimator to use: "WLSMV" for ordinal, "ML" for continuous (default: "WLSMV")
#' @param rotation Rotation method: "oblimin", "varimax", "promax", etc. (default: "oblimin")
#' @param parallel Use parallel processing (default: TRUE)
#' @param n_cores Number of cores to use (default: detectCores() - 1)
#' @param seed Random seed for reproducibility (default: 12345)
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return An object of class "mcSEM" containing:
#' \itemize{
#'   \item n_recommended: Recommended sample size
#'   \item results: Data frame with results for each sample size
#'   \item details: List with sample size for each criterion
#'   \item config: Configuration parameters used
#'   \item cor_matrix: Correlation matrix used for simulation
#'   \item plot: ggplot2 object (4-panel plot)
#' }
#'
#' @examples
#' \dontrun{
#' # Run EFA sample size analysis with 3 factors
#' result <- mc_efa(
#'   data = mydata,
#'   n_factors = 3,
#'   items = paste0("item", 1:15),
#'   rotation = "oblimin"
#' )
#'
#' # View results
#' print(result)
#'
#' # Save plot
#' ggsave("efa_sample_size.png", result$plot, width = 12, height = 9)
#' }
#'
#' @export
mc_efa <- function(data,
                   n_factors,
                   items = NULL,
                   n_range = seq(100, 500, by = 50),
                   reps = 500,
                   criterion = "moderate",
                   power_target = 0.80,
                   cor_type = "polychoric",
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

  # Get thresholds
  thresholds <- .get_thresholds(criterion)

  # Extract items
  if (is.null(items)) {
    item_names <- names(data)
  } else {
    item_names <- items
  }

  # Validate items exist in data
  missing_items <- item_names[!item_names %in% names(data)]
  if (length(missing_items) > 0) {
    stop(sprintf("Items not found in data: %s", paste(missing_items, collapse = ", ")))
  }

  data_items <- data[, item_names, drop = FALSE]
  n_items <- ncol(data_items)
  n_obs <- nrow(data_items)

  if (verbose) {
    cat("\n")
    cat("================================================================\n")
    cat("   MONTE CARLO SAMPLE SIZE ANALYSIS FOR EFA (A POSTERIORI)\n")
    cat("================================================================\n\n")
    cat(sprintf("Data: %d observations, %d items\n", n_obs, n_items))
    cat(sprintf("Factors to extract: %d\n", n_factors))
    cat(sprintf("Correlation type: %s\n", toupper(cor_type)))
    cat(sprintf("Estimator: %s\n", estimator))
    cat(sprintf("Rotation: %s\n", rotation))
    cat(sprintf("Criterion: %s (CFI >= %.2f, RMSEA <= %.3f, SRMR <= %.3f)\n",
                criterion, thresholds$cfi, thresholds$rmsea, thresholds$srmr))
    cat(sprintf("Power target: %.0f%%\n", power_target * 100))
    cat(sprintf("Replications: %d per sample size\n", reps))
    cat(sprintf("Sample sizes: %d to %d\n\n", min(n_range), max(n_range)))
  }

  # Calculate correlation matrix
  if (cor_type == "polychoric") {
    if (verbose) cat("Calculating polychoric correlation matrix...\n")
    cor_matrix <- .calc_polychoric(data_items, verbose = verbose)
  } else {
    cor_matrix <- stats::cor(data_items, use = "pairwise.complete.obs")
  }

  # Convert to covariance (assuming standardized)
  sigma <- cor_matrix

  if (verbose) {
    cat("\n")
    cat("================================================================\n")
    cat("   RUNNING MONTE CARLO SIMULATION\n")
    if (parallel) cat(sprintf("   Using %d cores in parallel\n", n_cores))
    cat("================================================================\n\n")
  }

  # Run simulation for each sample size
  all_results <- list()

  for (n in n_range) {
    if (verbose) cat(sprintf("  N = %3d ", n))

    result_n <- .simulate_for_n_efa(
      n = n,
      reps = reps,
      sigma = sigma,
      n_factors = n_factors,
      n_items = n_items,
      n_categories = if (estimator == "WLSMV") 5 else NULL,
      estimator = estimator,
      rotation = rotation,
      thresholds = thresholds,
      parallel = parallel,
      n_cores = n_cores,
      seed = seed + n,
      item_names = item_names
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
  subtitle <- sprintf("Factors: %d | Rotation: %s | Criterion: %s | %d replications | %s correlation",
                      n_factors, rotation, criterion, reps, cor_type)
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
    config = list(
      type = "efa_a_posteriori",
      analysis = "EFA",
      n_factors = n_factors,
      n_items = n_items,
      n_obs_original = n_obs,
      criterion = criterion,
      thresholds = thresholds,
      cor_type = cor_type,
      estimator = estimator,
      rotation = rotation,
      reps = reps,
      power_target = power_target
    ),
    cor_matrix = cor_matrix,
    plot = plot_obj
  )

  class(result) <- c("mcSEM", "list")

  result
}
