#' A 'ggplot2' geom to fit text inside a circle
#'
#' `geom_circumscribe()` shrinks, grows, and wraps text to fit inside a circle.
#'
#' @details
#'
#' Except where noted, `geom_circumscribe()` behaves like
#' `ggplot2::geom_text()`.
#'
#' If the text is too big to fit in the circle, it will shrink to fit the
#' circle. If `grow = TRUE` is set, the text will be made as large as possible
#' whether that means shrinking or growing it.
#'
#' If `reflow = TRUE` is set, the text will be reflowed (wrapped) to fill the
#' circle as tightly as possible. If the text is still too big for the circle
#' after reflowing, it will be shrunk as usual. Existing line breaks are
#' respected when reflowing.
#'
#' For now, radius is expressed as an absolute parameter rather than a plot
#' aesthetic. This is liable to change in future.
#'
#' @section Aesthetics:
#'
#' - label (required)
#' - x (required)
#' - y (required)
#' - alpha
#' - angle
#' - colour
#' - family
#' - fontface
#' - lineheight
#' - fontsize
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... Standard
#' geom arguments as for `ggplot2::geom_text()`.
#' @param radius Radius of the circle. A `grid::unit()` object. Defaults to 50
#' mm.
#' @param padding Padding between the text and the circle. A `grid::unit()`
#' object. Defaults to `grid::unit(4, "mm")`.
#' @param grow If `TRUE`, text will be made as large as able to fit in the
#' circle.
#' @param reflow If `TRUE`, text will be reflowed (wrapped) to fill the circle
#' as tightly as possible.
#'
#' @export
geom_circumscribe <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  radius = grid::unit(50, "mm"),
  padding = grid::unit(4, "mm"),
  grow = FALSE,
  reflow = FALSE,
  ...
) {
  ggplot2::layer(
    geom = GeomCircumscribe,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      radius = radius,
      padding = padding,
      grow = grow,
      reflow = reflow,
      ...
    )
  )
}

#' GeomCircumscribe
#' @noRd
GeomCircumscribe <- ggplot2::ggproto(
  "GeomCircumscribe",
  ggplot2::Geom,
  required_aes = c("label", "x", "y"),
  default_aes = ggplot2::aes(
    alpha = 1,
    angle = 0,
    colour = "black",
    family = "",
    fontface = 1,
    lineheight = 1.4,
    fontsize = 12
  ),

  setup_params = function(data, params) {
    params
  },

  setup_data = function(data, params) {
    data
  },

  draw_key = ggplot2::draw_key_label,

  draw_panel = function(
    data,
    panel_scales,
    coord,
    radius = grid::unit(50, "mm"),
    padding = grid::unit(4, "mm"),
    grow = FALSE,
    reflow = FALSE
  ) {

    # Transform data to plot scales
    data <- coord$transform(data, panel_scales)

    # Set up gTree
    gt <- grid::gTree(
      data = data,
      radius = radius,
      padding = padding,
      grow = grow,
      reflow = reflow,
      cl = "circumscribetree"
    )
    gt$name <- grid::grobName(gt, "geom_circumscribe")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.circumscribetree <- function(gt) {

  data <- gt$data

  # Effective radius available to the text: the circle radius less padding, in
  # mm. Converting the units here (rather than coercing with as.numeric) lets
  # radius and padding be given in any absolute unit.
  radius_mm <- grid::convertWidth(gt$radius, "mm", valueOnly = TRUE)
  padding_mm <- grid::convertWidth(gt$padding, "mm", valueOnly = TRUE)
  effective_radius <- radius_mm - padding_mm

  # Build the line grobs for one label
  label_grobs <- function(i) {
    text <- data[i, ]

    tokens <- tokenise_label(text$label)
    if (length(tokens$words) == 0 || effective_radius <= 0) return(NULL)

    # Measure words and font metrics at the aesthetic font size (the reference
    # size); everything else scales linearly from here.
    gp_reference <- grid::gpar(
      fontsize = text$fontsize,
      fontfamily = text$family,
      fontface = text$fontface
    )
    metrics <- font_metrics_mm(gp_reference)
    height <- metrics$height
    leading <- text$lineheight * height
    widths <- vapply(tokens$words, string_width_mm, double(1),
                     gp = gp_reference)

    fit <- fit_label(widths, tokens$seg, metrics$space, height, leading,
                     effective_radius, grow = gt$grow, reflow = gt$reflow)

    # Place each line: horizontally centred on the circle, and vertically
    # centred in its band. The band centre sits at (top - height/2) above the
    # circle centre at the reference size; scale to the fitted size.
    lapply(seq_len(nrow(fit$lines)), function(j) {
      line <- fit$lines[j, ]
      label <- paste(tokens$words[line$first:line$last], collapse = " ")
      offset <- (line$top - height / 2) * fit$sigma
      grid::textGrob(
        label = label,
        x = grid::unit(text$x, "npc"),
        y = grid::unit(text$y, "npc") + grid::unit(offset, "mm"),
        gp = grid::gpar(
          alpha = text$alpha,
          col = text$colour,
          fontsize = text$fontsize * fit$sigma,
          fontfamily = text$family,
          fontface = text$fontface
        )
      )
    })
  }

  textgrobs <- unlist(lapply(seq_len(nrow(data)), label_grobs),
                      recursive = FALSE)
  class(textgrobs) <- "gList"
  grid::setChildren(gt, textgrobs)
}
