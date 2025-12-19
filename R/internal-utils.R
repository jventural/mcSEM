# =============================================================================
# INTERNAL UTILITY FUNCTIONS
# =============================================================================

#' Calculate CFA model parameters
#' @noRd
.calc_model_params <- function(n_factors, n_items, items_per_factor) {
  unique_obs <- (n_items * (n_items + 1)) / 2
  n_loadings <- n_items - n_factors

n_error_vars <- n_items
  n_factor_covs <- (n_factors * (n_factors - 1)) / 2
  free_params <- n_loadings + n_error_vars + n_factor_covs
  df <- unique_obs - free_params

  list(
    unique_obs = unique_obs,
    free_params = free_params,
    df = df,
    n_loadings = n_loadings,
    n_error_vars = n_error_vars,
    n_factor_covs = n_factor_covs
  )
}


#' Define fit index thresholds based on criterion
#' @noRd
.get_thresholds <- function(criterion = "moderate") {
  switch(criterion,
         "strict" = list(cfi = 0.95, rmsea = 0.05, srmr = 0.06),
         "moderate" = list(cfi = 0.95, rmsea = 0.06, srmr = 0.08),
         "flexible" = list(cfi = 0.90, rmsea = 0.08, srmr = 0.10),
         list(cfi = 0.95, rmsea = 0.06, srmr = 0.08)
  )
}


#' Configure misspecification parameters
#' @noRd
.config_misspecification <- function(level, cross_loadings = NULL, residual_cors = NULL) {
  defaults <- list(
    "none" = list(cross_loadings = 0.00, residual_cors = 0.00, prop_residuals = 0.00),
    "minor" = list(cross_loadings = 0.10, residual_cors = 0.10, prop_residuals = 0.10),
    "moderate" = list(cross_loadings = 0.15, residual_cors = 0.15, prop_residuals = 0.15),
    "severe" = list(cross_loadings = 0.25, residual_cors = 0.25, prop_residuals = 0.20)
  )

  params <- defaults[[level]]
  if (is.null(params)) params <- defaults[["moderate"]]

  if (!is.null(cross_loadings)) params$cross_loadings <- cross_loadings
  if (!is.null(residual_cors)) params$residual_cors <- residual_cors

  params
}


#' Create lavaan model syntax from factor structure
#' @noRd
.create_lavaan_syntax <- function(n_factors, items_per_factor, item_prefix = "item") {
  lines <- c()
  item_idx <- 1

  for (f in 1:n_factors) {
    items <- paste0(item_prefix, item_idx:(item_idx + items_per_factor[f] - 1))
    line <- paste0("F", f, " =~ ", paste(items, collapse = " + "))
    lines <- c(lines, line)
    item_idx <- item_idx + items_per_factor[f]
  }

  paste(lines, collapse = "\n")
}


#' Create loading matrix with optional misspecification
#' @noRd
.create_loading_matrix <- function(n_factors, items_per_factor, loadings,
                                    cross_loading_mag = 0) {
  n_items <- sum(items_per_factor)
  lambda <- matrix(0, nrow = n_items, ncol = n_factors)

  # Main loadings with slight variation
  if (length(loadings) == 1) {
    all_loadings <- loadings + stats::runif(n_items, -0.08, 0.08)
    all_loadings <- pmax(0.45, pmin(0.85, all_loadings))
  } else {
    all_loadings <- loadings
  }

  # Assign main loadings
  item_idx <- 1
  for (f in 1:n_factors) {
    for (i in 1:items_per_factor[f]) {
      lambda[item_idx, f] <- all_loadings[item_idx]
      item_idx <- item_idx + 1
    }
  }

  # Add cross-loadings (misspecification)
  if (cross_loading_mag > 0 && n_factors > 1) {
    n_cross <- max(1, round(n_items * 0.3))
    items_with_cross <- sample(1:n_items, n_cross)

    for (item in items_with_cross) {
      main_factor <- which(lambda[item, ] > 0.3)[1]
      other_factors <- setdiff(1:n_factors, main_factor)
      if (length(other_factors) > 0) {
        cross_factor <- sample(other_factors, 1)
        lambda[item, cross_factor] <- cross_loading_mag + stats::runif(1, -0.05, 0.05)
      }
    }
  }

  rownames(lambda) <- paste0("item", 1:n_items)
  colnames(lambda) <- paste0("F", 1:n_factors)

  lambda
}


#' Create Phi matrix (factor correlations)
#' @noRd
.create_phi_matrix <- function(n_factors, factor_cors) {
  phi <- diag(n_factors)

  if (length(factor_cors) == 1) {
    phi[lower.tri(phi)] <- factor_cors
    phi[upper.tri(phi)] <- t(phi)[upper.tri(phi)]
  } else if (is.matrix(factor_cors)) {
    phi <- factor_cors
  }

  colnames(phi) <- rownames(phi) <- paste0("F", 1:n_factors)
  phi
}


#' Create residual correlation matrix
#' @noRd
.create_residual_matrix <- function(n_items, residual_cor_mag = 0, prop_correlated = 0) {
  theta <- diag(n_items)

  if (residual_cor_mag > 0 && prop_correlated > 0) {
    total_pairs <- (n_items * (n_items - 1)) / 2
    n_correlated_pairs <- max(1, round(total_pairs * prop_correlated))

    all_pairs <- utils::combn(1:n_items, 2)
    selected_pairs <- all_pairs[, sample(ncol(all_pairs),
                                          min(n_correlated_pairs, ncol(all_pairs)))]

    if (is.vector(selected_pairs)) {
      selected_pairs <- matrix(selected_pairs, nrow = 2)
    }

    for (p in 1:ncol(selected_pairs)) {
      i <- selected_pairs[1, p]
      j <- selected_pairs[2, p]
      cor_val <- residual_cor_mag + stats::runif(1, -0.05, 0.05)
      theta[i, j] <- cor_val
      theta[j, i] <- cor_val
    }
  }

  theta
}


#' Generate population covariance matrix
#' @noRd
.generate_sigma <- function(lambda, phi, theta_cor = NULL) {
  n_items <- nrow(lambda)

  lambda_phi_lambda <- lambda %*% phi %*% t(lambda)
  error_vars <- 1 - diag(lambda_phi_lambda)
  error_vars <- pmax(0.1, error_vars)

  theta <- diag(error_vars)

  # Add residual correlations if provided
  if (!is.null(theta_cor) && any(theta_cor[lower.tri(theta_cor)] != 0)) {
    for (i in 1:(n_items - 1)) {
      for (j in (i + 1):n_items) {
        if (theta_cor[i, j] != 0) {
          cov_ij <- theta_cor[i, j] * sqrt(error_vars[i]) * sqrt(error_vars[j])
          theta[i, j] <- cov_ij
          theta[j, i] <- cov_ij
        }
      }
    }
  }

  sigma <- lambda_phi_lambda + theta

  # Ensure positive definite
  eigen_vals <- eigen(sigma, symmetric = TRUE)$values
  if (any(eigen_vals <= 0)) {
    sigma <- sigma + diag(n_items) * (abs(min(eigen_vals)) + 0.01)
  }

  sigma
}


#' Calculate polychoric correlation matrix
#' @noRd
.calc_polychoric <- function(data, verbose = FALSE) {
  data <- as.data.frame(lapply(data, as.numeric))

  tryCatch({
    result <- psych::polychoric(data, correct = 0.1)
    cor_matrix <- result$rho

    if (verbose) {
      message(sprintf("Polychoric correlation calculated. Range: [%.3f, %.3f]",
                      min(cor_matrix[lower.tri(cor_matrix)]),
                      max(cor_matrix[lower.tri(cor_matrix)])))
    }

    cor_matrix
  }, error = function(e) {
    warning("Polychoric correlation failed. Using Pearson correlation.")
    stats::cor(data, use = "pairwise.complete.obs")
  })
}
