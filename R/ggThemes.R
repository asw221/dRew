
## theme_calm <- function(...) {
##   theme_gray() +
##   theme(
##     text = element_text(family = "Trebuchet MS", color = "#22211d"),
##     strip.background = element_blank(),
##     panel.grid.minor = element_line(color = "#ffffff", size = 0.2),
##     panel.grid.major = element_line(color = "#ffffff", size = 0.4),
##     ## panel.grid.minor = element_line(color = "#f5f5f2", size = 0.2),
##     ## panel.grid.major = element_line(color = "#f5f5f2", size = 0.4),
##     ## panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
##     ## panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
##     ## panel.grid.minor = element_blank(),
##     ## panel.background = element_rect(fill = "#ebebe5", color = NA),
##     ## plot.background = element_rect(fill = "#f5f5f2", color = NA),
##     panel.background = element_rect(fill = "#f5f5f2", color = NA),
##     ## legend.background = element_rect(fill = "#f5f5f2", color = NA),
##     ## panel.border = element_blank(),
##     ...
##   )
## }



theme_calm <- function(
  text = element_text(family = "Trebuchet MS", color = "#22211d"),
  strip.background = element_blank(),
  panel.grid.minor = element_line(color = "#ffffff", size = 0.2),
  panel.grid.major = element_line(color = "#ffffff", size = 0.4),
  panel.background = element_rect(fill = "#f5f5f2", color = NA),
  ...
) {
  theme_gray() +
  theme(
    text = text,
    strip.background = strip.background,
    panel.grid.minor = panel.grid.minor,
    panel.grid.major = panel.grid.major,
    panel.background = panel.background,
    ...
  )
}




## Basic background-free theme similar to base R
theme_base <- function(
  text = element_text(family = "Trebuchet MS", color = "#22211d"),
  strip.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  ...
) {
  theme_bw() +
  theme(
    text = text,
    strip.background = strip.background,
    panel.grid.minor = panel.grid.minor,
    panel.grid.major = panel.grid.major,
    ...
  )
}

