

#' @title is_null_string
#'
#' @param w a string. Either "NULL" or any other "string".
#'
#' @return TRUE or FALSE. Returns TRUE if the string is "NULL"
#' @export
#' @note for use when writing functions with dplyr where function needs string as input.
#' @examples
#' is_null_string("NULL")
#' is_null_string("Something_else")
is_null_string <- function(w) {
  if (w == "NULL") {
    x <- TRUE
  } else {
    x <- FALSE
  }
  return(x)
}
