#' @title Version function for the clanCRC32 library
#' @description Version function for the clanCRC32 library.
#' @usage
#' clanCRC32()
#' @name clanCRC32
#' @author Claus E. Andersen
#' @return A list of information about the version and functions within clanCRC32.
#' @export
clanLattice <- function(){
  list(name="clanCRC32",
       version=0.001,
       date="March 29, 2020",
       functions=sort(c("clanCRC32")))
}
