# =============================================================================
# INTERNAL SIMULATION FUNCTIONS
# =============================================================================

#' Simulate ordinal data from covariance matrix
#' @noRd
.simulate_ordinal_data <- function(n, sigma, n_categories, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_items <- nrow(sigma)

  # Generate continuous multivariate normal data
  continuous_data <- MASS::mvrnorm(n, mu = rep(0, n_items), Sigma = sigma)

  # Define thresholds for categorization
  if (n_categories == 4) {
    thresholds <- stats::qnorm(c(0.20, 0.50, 0.80))
  } else if (n_categories == 5) {
    thresholds <- stats::qnorm(c(0.15, 0.35, 0.65, 0.85))
  } else if (n_categories == 7) {
    thresholds <- stats::qnorm(seq(1/7, 6/7, length.out = 6))
  } else {
    thresholds <- stats::qnorm(seq(1/n_categories, 1 - 1/n_categories,
                                   length.out = n_categories - 1))
  }

  # Discretize
  ordinal_data <- matrix(1, nrow = n, ncol = n_items)
  for (j in 1:n_items) {
    for (k in seq_along(thresholds)) {
      ordinal_data[continuous_data[, j] > thresholds[k], j] <- k + 1
    }
  }

  colnames(ordinal_data) <- paste0("item", 1:n_items)
  as.data.frame(ordinal_data)
}


#' Simulate continuous data from covariance matrix
#' @noRd
.simulate_continuous_data <- function(n, sigma, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_items <- nrow(sigma)
  data <- MASS::mvrnorm(n, mu = rep(0, n_items), Sigma = sigma)
  colnames(data) <- paste0("item", 1:n_items)

  as.data.frame(data)
}


#' Run a single simulation replication
#' @noRd
.run_single_rep <- function(i, n, sigma, model_syntax, n_categories,
                             estimator, seed, item_names = NULL) {
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

    # Fit model
    if (estimator == "WLSMV") {
      fit <- lavaan::cfa(model_syntax, data = sim_data,
                         ordered = TRUE, estimator = "WLSMV")
    } else {
      fit <- lavaan::cfa(model_syntax, data = sim_data, estimator = estimator)
    }

    # Check convergence
    if (lavaan::lavInspect(fit, "converged")) {
      result$converged <- TRUE
      fm <- lavaan::fitmeasures(fit, c("cfi", "tli", "rmsea", "srmr"))
      result$cfi <- fm["cfi"]
      result$tli <- fm["tli"]
      result$rmsea <- fm["rmsea"]
      result$srmr <- fm["srmr"]
    }

  }, error = function(e) {
    # Keep defaults
  }, warning = function(w) {
    # Ignore warnings
})

  result
}


#' Run simulation for a specific sample size
#' @noRd
.simulate_for_n <- function(n, reps, sigma, model_syntax, n_categories,
                             estimator, thresholds, parallel, n_cores, seed,
                             item_names = NULL) {

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl,
                            c("n", "sigma", "model_syntax", "n_categories",
                              "estimator", "seed", "item_names"),
                            envir = environment())

    # Export internal functions
    parallel::clusterExport(cl,
                            c(".run_single_rep", ".simulate_ordinal_data",
                              ".simulate_continuous_data"),
                            envir = asNamespace("mcSEM"))

    parallel::clusterEvalQ(cl, {
      library(MASS)
      library(lavaan)
    })

    results_list <- parallel::parLapply(cl, 1:reps, function(i) {
      .run_single_rep(i, n, sigma, model_syntax, n_categories, estimator, seed, item_names)
    })

    parallel::stopCluster(cl)

  } else {
    results_list <- lapply(1:reps, function(i) {
      .run_single_rep(i, n, sigma, model_syntax, n_categories, estimator, seed, item_names)
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


#' Run simulation with progress bar
#' @noRd
.simulate_for_n_pb <- function(n, reps, sigma, model_syntax, n_categories,
                                estimator, thresholds, parallel, n_cores, seed,
                                item_names = NULL) {

  if (parallel && requireNamespace("pbapply", quietly = TRUE) &&
      requireNamespace("parallel", quietly = TRUE)) {

    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl,
                            c("n", "sigma", "model_syntax", "n_categories",
                              "estimator", "seed", "item_names"),
                            envir = environment())

    parallel::clusterExport(cl,
                            c(".run_single_rep", ".simulate_ordinal_data",
                              ".simulate_continuous_data"),
                            envir = asNamespace("mcSEM"))

    parallel::clusterEvalQ(cl, {
      library(MASS)
      library(lavaan)
    })

    results_list <- pbapply::pblapply(1:reps, function(i) {
      .run_single_rep(i, n, sigma, model_syntax, n_categories, estimator, seed, item_names)
    }, cl = cl)

    parallel::stopCluster(cl)

  } else if (requireNamespace("pbapply", quietly = TRUE)) {
    results_list <- pbapply::pblapply(1:reps, function(i) {
      .run_single_rep(i, n, sigma, model_syntax, n_categories, estimator, seed, item_names)
    })
  } else {
    results_list <- lapply(1:reps, function(i) {
      .run_single_rep(i, n, sigma, model_syntax, n_categories, estimator, seed, item_names)
    })
  }

  # Extract and summarize results (same as .simulate_for_n)
  converged <- sapply(results_list, function(x) x$converged)
  cfi <- sapply(results_list, function(x) x$cfi)
  rmsea <- sapply(results_list, function(x) x$rmsea)
  srmr <- sapply(results_list, function(x) x$srmr)

  conv_idx <- which(converged)

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
