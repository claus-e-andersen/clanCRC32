#' @title convert.hex rrrrRobust prepanel function for lattice plots (xlim).
#' @description
#' This prepanel function can provide useful scales even if there is
#' only one data point or if all values are identical (no variability).
#' A limit parameter equal to 0.01 means that the scale will at least
#' go from 1-limit = 0.99 to 1+limit = 1.01 times the reference value.
#' The parameter limit.robust is for the situation when there is only one data point
#' that is not NA. The parameter limit.mean uses the mean as reference, and
#' limit.median uses the median value as reference.
#' Note that the lattice plots generally fail (without a prepanel function) if there is only
#' one single point and if that value is very large (for example, y=6e66).
#' The following prepanel functions can be used to handle such situations.
#' The prepanel function can also be used to ascertain that a useful
#' minimum scale of say +/- 0.5\% is always shown. Note that the actual
#' scale may be larger (i.e. the selected scale limits will always include the
#' data).
#' See also \link{prepanel.robust.y} and \link{prepanel.robust.xy}
#' Example:
#'
#' @usage none
#' @name convert.hex
#' @author Claus E. Andersen
#' @return a list with xlim-values for a trellis plot.
#' @param x gives the x-values.
#' @param y gives the y-values.
#' @param ... for additional parameters.
#' @param limit.robust limit to use is there is only one data point.
#' @param limit.mean is the mean-based minimum limit.
#' @param limit.median is the median-based minimum limit.
#' @export convert.hex
convert.hex <- function(x) {
  # This function converts hex strings to integers
  # without breaking the 2^31 limit.
  # Claus E. Andersen
  # January 13, 2015
  y <- strtoi(strsplit(x, "")[[1]],base=16)
  sum(y * (16)^rev((seq_along(y)-1)))
}
