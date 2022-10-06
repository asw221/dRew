

## See: https://lingpipe-blog.com/2012/06/08/autocorrelation-fft-kiss-eigen/
##      https://etudes.tibonihoo.net/literate_musing/autocorrelations.html


acov <- function(x, .trim = TRUE) {
  N <- length(x)
  NN <- 2^ceiling(log2(N))
  ## NN <- N
  x0 <- c(x - mean(x, na.rm = TRUE), rep(0, NN - N))
  z <- fft(x0)
  ac <- Re( fft(z * Conj(z), inverse = TRUE) ) / NN
  if (.trim)  ac[1:N]  else  ac
}


acor <- function(x, .trim = TRUE) {
  ac <- acov(x, .trim)
  ac / ac[1]
}



ess_univar <- function(
  x,
  method = c("threshold",  # Threshold ACF
             "geyer",      # Geyer [1992], Stat. Sci.'s ACF method
             "spectral"    # Power spectrum method (adapted survey sampling)
                              # [Liu, Chen, Wong, 1998, JASA]
             ),
  .threshold = 0
) {
  method <- match.arg(method)
  trim <- method == "spectral"
  rho <- suppressWarnings(acor(x, trim))
  if (any(is.na(rho))) {
    warning ("ACF contains NA values")
    return (structure(NA, M = NA, psd0 = NA))
  }
  M <- 0
  psd0 <- NA
  if (method == "threshold") {
    M <- suppressWarnings(min(which(rho > .threshold &
                                    c(rho[-1], Inf) <= .threshold)))
    if (!is.finite(M))
      M <- length(x)
    if (M > 1)
      nhat <- length(x) / (1 + 2 * sum(rho[2:M], na.rm = TRUE))
    else
      nhat <- length(x)
  } else if (method == "geyer") {
    condition <- FALSE
    while (!condition && (2 * M + 2) <= length(rho)) {
      condition <- 0 >= (rho[2 * M + 1] + rho[2 * M + 2])
      M <- M + 1
    }
    M <- 2 * max(M - 1, 1) + 2
    if (M > 1)
      nhat <- length(x) / (1 + 2 * sum(rho[2:M], na.rm = TRUE))
    else
      nhat <- length(x)
  } else if (method == "spectral") {
    psd <- Re(fft(rho))  # Imaginary part numerical error
    psd0 <- abs(psd[1])
    nhat <- length(x) * var(x, na.rm = TRUE) / psd0
  }
  structure(nhat, M = M, psd0 = psd0)
}


ess <- function(x, ...) UseMethod("ess", x)
ess.default <- function(x, ...)  unname(apply(x, 2, ess_univar, ...))
ess.matrix <- function(x, ...)  unname(apply(x, 2, ess_univar, ...))
ess.numeric <- function(x, ...)  ess_univar(x, ...)




## #' Effective sample size
## setGeneric("ess", function(object, ...) standardGeneric("ess"))
## setGeneric("ess", useAsDefault = TRUE,
##            function(object, ...) { ess_univar(object, ...) })
## setGeneric("ess", signature = "matrix",
##            function(object, ...) { apply(object, 2, ess_univar, ...) })


