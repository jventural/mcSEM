#' Print method for mcSEM objects
#'
#' @param x An object of class "mcSEM"
#' @param ... Additional arguments (ignored)
#'
#' @export
print.mcSEM <- function(x, ...) {

  cat("\n")
  cat("================================================================\n")
  cat("           mcSEM: Monte Carlo Sample Size Analysis\n")
  cat("================================================================\n\n")

  # Type of analysis
  if (x$config$type == "a_priori") {
    cat("Type: A Priori (theoretical model)\n")
    cat(sprintf("Model: %d factors, %d items\n",
                x$model$n_factors, x$model$n_items))
    cat(sprintf("Misspecification: %s\n", x$config$misspecification))
  } else {
    cat("Type: A Posteriori (with data)\n")
    cat(sprintf("Items: %d\n", x$config$n_items))
    cat(sprintf("Original N: %d\n", x$config$n_obs_original))
    cat(sprintf("Correlation: %s\n", x$config$cor_type))
  }

  cat(sprintf("Estimator: %s\n", x$config$estimator))
  cat(sprintf("Replications: %d\n", x$config$reps))
  cat("\n")

  # Criterion
  cat("Fit Criteria:\n")
  cat(sprintf("  CFI >= %.2f\n", x$config$thresholds$cfi))
  cat(sprintf("  RMSEA <= %.3f\n", x$config$thresholds$rmsea))
  cat(sprintf("  SRMR <= %.3f\n", x$config$thresholds$srmr))
  cat(sprintf("  Power target: %.0f%%\n", x$config$power_target * 100))
  cat("\n")

  # Minimum N for each criterion
  cat("Minimum N for 80% Power:\n")
  cat(sprintf("  Convergence: %s\n",
              ifelse(is.na(x$details$n_convergence), "Not achieved",
                     as.character(x$details$n_convergence))))
  cat(sprintf("  CFI: %s\n",
              ifelse(is.na(x$details$n_cfi), "Not achieved",
                     as.character(x$details$n_cfi))))
  cat(sprintf("  RMSEA: %s\n",
              ifelse(is.na(x$details$n_rmsea), "Not achieved",
                     as.character(x$details$n_rmsea))))
  cat(sprintf("  SRMR: %s\n",
              ifelse(is.na(x$details$n_srmr), "Not achieved",
                     as.character(x$details$n_srmr))))
  cat("\n")

  # Recommendation
  cat("================================================================\n")
  cat(sprintf("  RECOMMENDED SAMPLE SIZE: %d PARTICIPANTS\n", x$n_recommended))
  cat("================================================================\n\n")

  cat("Use result$results for detailed table\n")
  cat("Use result$plot for visualization\n")
  cat("Use ggsave('filename.png', result$plot) to save plot\n\n")

  invisible(x)
}


#' Plot method for mcSEM objects
#'
#' @param x An object of class "mcSEM"
#' @param type Plot type: "full" (4 panels) or "simple" (1 panel)
#' @param ... Additional arguments passed to print
#'
#' @export
plot.mcSEM <- function(x, type = "full", ...) {

  if (type == "simple") {
    subtitle <- sprintf("Criterion: %s | Replications: %d",
                        x$config$criterion, x$config$reps)
    p <- .create_power_plot(x$results, x$n_recommended,
                            x$config$thresholds, subtitle = subtitle)
  } else {
    p <- x$plot
  }

  print(p)
  invisible(p)
}


#' Summary method for mcSEM objects
#'
#' @param object An object of class "mcSEM"
#' @param ... Additional arguments (ignored)
#'
#' @export
summary.mcSEM <- function(object, ...) {

  cat("\n=== Results Table ===\n\n")
  print(round(object$results, 3))

  cat("\n=== Recommendation ===\n\n")
  cat(sprintf("Recommended N: %d\n", object$n_recommended))

  invisible(object)
}
