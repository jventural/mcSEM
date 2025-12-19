.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    "================================================================\n",
    "  mcSEM: Monte Carlo Sample Size Estimation for SEM\n",
    "  Version ", utils::packageVersion("mcSEM"), "\n",
    "================================================================\n",
    "\n",
    "Main functions:\n",
    "  mc_cfa()         - A posteriori analysis (with data)\n",
    "  mc_cfa_apriori() - A priori analysis (theoretical model)\n",
    "\n",
    "For help: ?mc_cfa or ?mc_cfa_apriori\n"
  )
}

.onLoad <- function(libname, pkgname) {
  # Set default options if needed
  op <- options()
  op.mcSEM <- list(
    mcSEM.verbose = TRUE,
    mcSEM.parallel = TRUE
  )
  toset <- !(names(op.mcSEM) %in% names(op))
  if (any(toset)) options(op.mcSEM[toset])

  invisible()
}
