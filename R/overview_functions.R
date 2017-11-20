

#' over_vinta_fpd
#' @title Overview calculation for vintage analysis based on first pay date
#' @inheritParams vintalyse
#' @param date the latest point in time month of loan data available
#' @param default_definition the default definition applicable to the loan
#'   portfolio
#' @return a list containing default rate for this month and the % change in
#'   value from last month for period 4 and 12.
#' @export
#'
#' @examples
#' dft <- over_vinta_fpd(df, "2017-08-31", 4)
#'
over_vinta_fpd <- function(data, date, default_definition) {
  date12 <-    last_day(as.Date(date) %m-% months(12))
  datedef <-   last_day(as.Date(date) %m-% months(default_definition))
  date13 <-    last_day(as.Date(date) %m-% months(13))
  datedef1 <-  last_day(as.Date(date) %m-% months(default_definition+1))
  df_datefilter <- data %>%
    filter(fpd_month %in% c(date12, datedef, date13, datedef1))
  df_vinta <- vintalyse(data = df_datefilter, "fpd_period", "fpd_month")
  df_vinta <- df_vinta %>%
    filter(fpd_period %in% c(default_definition, 12))
  this_month_12 <- df_vinta %>%
    filter(fpd_month == date12 & fpd_period == 12) %>%
    select(default_perc_loan_amount)
  last_month_12 <- df_vinta %>%
    filter(fpd_month == date13 & fpd_period == 12) %>%
    select(default_perc_loan_amount)
  this_month_4 <- df_vinta %>%
    filter(fpd_month == datedef & fpd_period == default_definition) %>%
    select(default_perc_loan_amount)
  last_month_4 <- df_vinta %>%
    filter(fpd_month == datedef1 & fpd_period == default_definition) %>%
    select(default_perc_loan_amount)

  list("this_month_12" = this_month_12[[2]],
       "change_12" = this_month_12[[2]] / last_month_12[[2]] - 1,
       "this_month_4 " = this_month_4[[2]],
       "change_4" = ifelse(this_month_4[[2]] / last_month_4[[2]] - 1 == Inf,
                             "last month = 0", this_month_4[[2]] / last_month_4[[2]] - 1)
       )
}


#' over_pd0
#' @title Overview of 12 month probability of default for 0 in arrears
#' @inheritParams over_vinta_fpd
#'
#' @return a list containing the 12 month probability of default for the 0 in
#'   arrears segment for this month and the % change in value from last month.
#' @export
#'
#' @examples
#' over_pd0(df, "2017-03-31")
#'
over_pd0 <- function(data, date) {
  date12 <-    as.character(last_day(as.Date(date) %m-% months(12)))
  date13 <-    as.character(last_day(as.Date(date) %m-% months(13)))
  df <- df %>%
    filter(pointintime_month >= date13)
  df_pd <- pd(df, 1, 12, 1)
  this_month_pd0 <- df_pd  %>%
    filter(pointintime_month == date12) %>%
    select(probability_of_default)
  last_month_pd0 <- df_pd %>%
    filter(pointintime_month == date13) %>%
    select(probability_of_default)

  list("this_month_pd0" = this_month_pd0[[2]],
       "last_month_pd0" = last_month_pd0[[2]],
       "change_pd0" = this_month_pd0[[2]] / last_month_pd0[[2]] - 1)

}

#' over_sales
#' @title Overview of this month's sales and book size
#' @inheritParams over_vinta_fpd
#'
#' @return a list containing the loan sales number for this month, last month
#'   and the % change in value from last month.
#' @export
#'
#' @examples
#' dftest <- over_sales(df, "2017-06-30")
#' dfbk <- over_booksize(df, "2017-06-30")
#'
over_sales <- function(data, date) {
  date1 <-    as.character(last_day(as.Date(date) %m-% months(1)))
  dfmonths <- df %>%
    filter(orig_month >= date1)
  dfsales <- dfmonths %>%
    group_by(orig_month) %>%
    summarise(loan_sales = sum(loan_amount))
  this_month_sales <- dfsales %>%
    filter(orig_month == date) %>%
    select(loan_sales)
  last_month_sales <- dfsales %>%
    filter(orig_month == date1) %>%
    select(loan_sales)
  # this_month_sales <- dfsales[dfsales$orig_month == date, "loan_sales"]
  # last_month_sales <- dfsales[dfsales$orig_month == date1, "loan_sales"]

  list("this_month_sales" = this_month_sales[[1]],
       "last_month_sales" = last_month_sales[[1]],
       "change_sales" = this_month_sales[[1]] / last_month_sales[[1]] - 1)
}


#' @describeIn over_sales Returns a list containing the book size number for
#'   this month, last month and the % change in value from last month
over_booksize <- function(data, date) {
  date1 <-    as.character(last_day(as.Date(date) %m-% months(1)))
  dfmonths <- df %>%
    filter(pointintime_month >= date1)
  dfbooksize <- dfmonths %>%
    group_by(pointintime_month) %>%
    summarise(booksize = sum(closing_balance))
  this_month_booksize <- dfbooksize[dfbooksize$pointintime_month == date,  "booksize"]
  last_month_booksize <- dfbooksize[dfbooksize$pointintime_month == date1, "booksize"]

  list("this_month_booksize" = this_month_booksize[[1]],
       "last_month_booksize" = last_month_booksize[[1]],
       "change_booksize" = this_month_booksize[[1]] / last_month_booksize[[1]] - 1)
}










