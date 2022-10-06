
#' Scientific notation for ggplot scales
#'
#' @param x  "Unformatted" numeric values
#' @return   Parsed `expression` objects
#' @examples
#' scientific10(1.2e2)    ## expression(1 %*% 10^+01)
scientific10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}



#' Thousands separators for ggplot scales
#'
#' @param x  Numeric input(s)
#' @return   Strings with thousands separators
#' @examples
#' thousands(1:5 * 1000)
thousands <- function(x, sep = ",") {
  replacement <- paste0("\\1", sep, "\\2")
  gsub("([0-9])(?=(?:[0-9]{3})+(?![0-9]))", replacement, as.character(x), perl = TRUE)
}
## https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch06s12.html


## df <- data.frame(x = 1:5 * 1000, y = rnorm(5))
## ggplot(df, aes(x, y)) +
##   geom_point() +
##   scale_x_continuous(labels = thousands)
