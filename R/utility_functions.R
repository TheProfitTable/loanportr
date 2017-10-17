

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


#' @title add_perc
#' @description adds percentage distribution columns to a summarised data frame
#' @param data A data frame that has a date or similar variable as its first
#'   column, followed by the levels of some categorical variable that makes up a
#'   total, which is the last column. Typically a data frame created by the
#'   \code{\link{vardistr_amnt}} function.
#'
#' @return the same data frame plus a column starting with "perc" for each of
#'   the existing variables. These added variables will add up to 100% if they
#'   are summed up row wise.
#' @export
#'
#' @examples
#' x <- add_perc(df_vda)
#'
add_perc <- function(data) {

  df_amnt <- data

  ncol <- ncol(df_amnt) - 1

  for (i in 2:ncol) {
    name  <- as.name(names(df_amnt[i])) # as name
    nname <- paste0("perc_", names(df_amnt[i])) # as string

    total <- quo(total)

    df_amnt <- mutate(df_amnt, !!nname := UQ(name) / UQ(total))
  }
  return(df_amnt)
}

