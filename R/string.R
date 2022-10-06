

#' Reverse characters in a string
#'
#' @param s  Input character string
#' @return   The input `s` in reverse order
#' @examples
#' strrev(c("foo", "bar"))
strrev <- function(s) {
  s <- strsplit(as.character(s), split = "")
  sapply(s, function(sss) paste(rev(sss), collapse = ""))
}

