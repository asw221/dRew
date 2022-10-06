
count <- function(x, ref = sort(unique(x)), use.names = FALSE ) {
  v <- vapply( ref, function(j) sum(x == j, na.rm = TRUE), integer(1) )
  if ( use.names ) {
    names (v) <- ref
  }
  v
}
