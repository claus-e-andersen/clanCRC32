#' @title Convert hex string to decimal number.
#' @description
#' # This function converts hex strings to integers
#' without breaking the 2^31 limit.
#' @usage convert.hex("FF")
#' @name convert.hex
#' @author Claus E. Andersen
#' @return integer
#' @param x input string (a hex number)
#' @export convert.hex
convert.hex <- function(x) {
  # This function converts hex strings to integers
  # without breaking the 2^31 limit.
  # Claus E. Andersen
  # January 13, 2015
  y <- strtoi(strsplit(x, "")[[1]],base=16)
  sum(y * (16)^rev((seq_along(y)-1)))
}# convert.hex
