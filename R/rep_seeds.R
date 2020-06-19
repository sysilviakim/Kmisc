#' Create Fully Reproducible Seeds
#'
#' Create fully reproducible seeds for cross-validation in some applications
#' of machine learning.
#'
#' @param folds Number of folds for cross-validation. Defaults to 10/
#' @param n Number of seeds to set per fold. Defaults to 10,000.
#' @param seed Initial seed. Defaults to 123.
#'
#' @export

rep_seeds <- function(folds = 10, n = 1e4, seed = 123) {
  set.seed(seed)
  ## (n_repeats * nresampling) + 1
  seeds <- vector(mode = "list", length = folds + 1)
  for (i in seq(folds)) {
    seeds[[i]] <- sample.int(n = n, n)
  }
  seeds[[folds + 1]] <- sample.int(n = n, 1)
  return(seeds)
}

