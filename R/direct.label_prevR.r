#' Direct label on a ggplot object
#'
#' Direct label a ggplot2 grouped plot
#'
#' @param p The ggplot object.
#' @param method Method for direct labeling
#'   (see [directlabels::direct.label()]).
#' @param debug Show debug output?
#'
#' @return The ggplot object with direct labels added.
#'
#' @note This function is based on and similar to
#' [directlabels::direct.label()]
#' except that legend is not hidden.
#'
#' @seealso [directlabels::direct.label()]
#'
#' @importFrom directlabels geom_dl
#' @importFrom directlabels default.picker
#' @export

direct.label_prevR <- function(p, method = NULL, debug = FALSE) {
# based on http://www.rdocumentation.org/packages/directlabels/functions/direct.label.ggplot #nolint
  getData <- function(colour.or.fill) {
    for (L in p$layers) {
      m <- p$mapping
      m[names(L$mapping)] <- L$mapping
      colvar <- m[[colour.or.fill]]
      if (!is.null(colvar)) {
        return(list(layer = L, colvar = colvar))
      }
    }
  }
  dl.info <- getData("colour")
  if (is.null(dl.info)) {
    dl.info <- getData("fill")
  }
  if (is.null(dl.info)) {
    stop("Need colour or fill aesthetic to infer default direct labels.")
  }
  L <- dl.info$layer
  colvar <- dl.info$colvar
  if (is.null(method)) {
    method <- directlabels::default.picker("ggplot")
  }
  data <- if ((!is.null(L$data)) && (length(L$data) > 0)) {
    L$data
  } else {
    NULL
  }
  a <- ggplot2::aes(label = {{ colvar }}, colour = {{ colvar }})
  a2 <- structure(c(L$mapping, a), class = "uneval")
  dlgeom <- directlabels::geom_dl(a2,
    method = method, stat = L$stat, debug = debug,
    data = data
  )
  dlgeom$stat_params <- L$stat_params
  leg.info <- NULL # We want to keep the legend
  guide.args <- as.list(rep("none", length(leg.info$hide)))
  names(guide.args) <- leg.info$hide
  guide.args$colour <- "none"
  guide <- do.call(ggplot2::guides, guide.args)
  p + dlgeom + guide
}
