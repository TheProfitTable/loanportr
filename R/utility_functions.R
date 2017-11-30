

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
#'   @param start_col start start summing across  from this column number
#'
#' @return the same data frame plus a column starting with "perc" for each of
#'   the existing variables. These added variables will add up to 100% if they
#'   are summed up row wise.
#' @export
#'
#' @examples
#' x <- add_perc(df_vda, 2)
#'
add_perc <- function(data, start_col) {

  df_amnt <- data

  ncol <- ncol(df_amnt) - 1

  for (i in start_col:ncol) {
    name  <- as.name(names(df_amnt[i])) # as name
    nname <- paste0("perc_", names(df_amnt[i])) # as string

    total <- quo(total)

    df_amnt <- mutate(df_amnt, !!nname := UQ(name) / UQ(total))
  }
  return(df_amnt)
}


#' @title string_to_quote
#' @description convert a string to a quoted expression within a function
#' @param string a character string that represents a variable or expression.
#'   The input parameter to a function.
#'
#' @return returns the quoted version of the string.
#' @export
#' @note Only use inside a function. Variable should be unquoted via UQ() or !!
#'   for use.
#' @examples
#' input <- string_to_quote(input) #where input is a parameter entered as string
#'
string_to_quote <- function(string) {
    str <- as.name(string)
    enquo(str)
}

#' max_var
#' @title maximum of a variable in a data frame
#'
#' @param data any data frame
#' @param var the name of a variable in the data frame upon which a maximum can be computed.
#'
#' @return the maximum of the variable
#' @export
#'
#' @examples
#' max_month(df, "fpd_month")
#'
max_var <- function(data, var) {
  dfs <- data %>%
      select(var)
  max(dfs[[var]])
}

#' @title last_day
#' @description calculate last day of the month given a date.
#' @param date
#'
#' @return last day of the month
#' @export
#'
#' @examples
#' last_day(date = as_date("2017-01-05"))
#'
last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}
