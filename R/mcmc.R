

#' Split RHat
#'
#' Univariate Split \eqn{\hat{R}}{R} diagnostic for MCMC convergence
#'
#' @references
#' Vehtari et al (2021) Bayesian Analysis. 16:2. pp. 667--718.
#' "Rank-normalization, folding, and localization: An improved
#' $\hat{R}$ for assessing convergence of MCMC."
#'
#' @param x  Numeric matrix of MCMC samples for a parameter. Each
#'   column indexes a separate MCMC chain and each row indexes an MCMC
#'   sample
#'
#' @return Estimated convergence diagnostic (scalar)
#'
split.rhat <- function(x) {
  N <- nrow(x)  # Number of samples per chain
  M <- ncol(x)  # Number of chains
  xbar.chains <- colMeans(x)
  xbar <- mean(xbar.chains)
  s <- apply(x, 2, var)
  ## Compute between and within chain variance
  B <- N / (M - 1) * sum( (xbar.chains - xbar)^2 )
  W <- sum(s) / M
  ## Compute (over-)estimate of posterior variance
  var.hat.plus <- (N - 1) / N * W + 1/N * B
  ## Compute Split-\hat{R}
  sqrt( var.hat.plus / W )
}



#' Rank-normalization
#'
#' @param x  Numeric vector
#' @return Normal quantile transformation of the ranks of \code{x}
rank.normalize <- function(x) {
  qnorm( (rank(x) - 0.375) / (length(x) + 0.25) )
}
