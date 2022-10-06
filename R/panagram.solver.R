

dictionary.file <- function() {
  file.path("", "usr", "share", "dict", "words")
}


panagram.solver <- function(panagram, n = 4:7, .max.repeats = 1) {
  require (data.table)
  require (gtools)
  .Dict <- tolower(c(as.matrix(fread(dictionary.file()))))
  panagram <- tolower(panagram)
  if ( length(panagram) == 1 )
    pgm <- sapply(1:nchar(panagram), function(i) substr(panagram, i, i))
  else
    pgm <- panagram
  pgmpgm <- rep(pgm, .max.repeats)
  perm <- lapply(n, function(nn)
    gtools::permutations(length(pgmpgm), nn, 1:(.max.repeats * length(pgm)))) %>%
    lapply(function(l) apply(l, 1, function(i) pgmpgm[i])) %>%
    lapply(function(l) apply(l, 2, paste, collapse = "", sep = "")) %>%
    do.call("c", .)
  valid <- sapply(perm, function(wd) any(.Dict == wd)) &
    grepl(pgm[1], perm, fixed = TRUE)
  sort(perm[valid])
}


