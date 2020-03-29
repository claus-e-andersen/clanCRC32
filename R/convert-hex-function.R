#' @title Convert hex strings to integers (needed in CRC32 computations)
#' Does not break the 2^31 limit.
#' @name convert.hex
#' @param Hex string to be converted.
#'
#' @return CRC32 value
#'
#' @export convert.hex
convert.hex <- function(x) {
  # This function converts hex strings to integers
  # without breaking the 2^31 limit.
  # Claus E. Andersen
  # January 13, 2015
  y <- strtoi(strsplit(x, "")[[1]],base=16)
  sum(y * (16)^rev((seq_along(y)-1)))
}
