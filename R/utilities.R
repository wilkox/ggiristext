# Text measurement, in millimetres, with caching.
#
# Every measurement is a graphics-device round-trip (grobWidth/grobHeight and
# a unit conversion), and makeContent re-runs on every redraw and resize, so
# results are memoised in a package-level cache keyed on the string and the
# graphical parameters and device that determine its size. After the first
# draw, refitting on resize costs no measurements at all. Words are measured at
# a single reference font size and scaled linearly, so a label drawn at any
# fitted size still costs one measurement per distinct word.

measurement_cache <- new.env(parent = emptyenv())

cache_key <- function(...) {
  paste(..., grDevices::dev.cur(), sep = "\r")
}

# Width of a string in mm at the given graphical parameters.
string_width_mm <- function(string, gp) {
  key <- cache_key("w", string, gp$fontfamily, gp$fontface, gp$fontsize)
  cached <- measurement_cache[[key]]
  if (!is.null(cached)) return(cached)
  width <- grid::convertWidth(
    grid::grobWidth(grid::textGrob(string, gp = gp)), "mm", valueOnly = TRUE
  )
  measurement_cache[[key]] <- width
  width
}

# Font metrics in mm at the given graphical parameters: the line height H
# (ascent + descent) from a reference string carrying full ascenders and
# descenders, so that line height is content-independent; and the inter-word
# space width, taken as a difference because a lone space measures unreliably
# on some devices.
font_metrics_mm <- function(gp) {
  key <- cache_key("m", gp$fontfamily, gp$fontface, gp$fontsize)
  cached <- measurement_cache[[key]]
  if (!is.null(cached)) return(cached)
  reference <- grid::textGrob("Mgjq", gp = gp)
  metrics <- list(
    height = grid::convertHeight(grid::grobHeight(reference), "mm",
                                 valueOnly = TRUE) +
             grid::convertHeight(grid::grobDescent(reference), "mm",
                                 valueOnly = TRUE),
    space = string_width_mm("x x", gp) - string_width_mm("xx", gp)
  )
  measurement_cache[[key]] <- metrics
  metrics
}
