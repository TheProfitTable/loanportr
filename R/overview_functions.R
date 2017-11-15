

#' over_vinta_fpd
#' @title Overview calculation for vintage analysis based on first pay date
#' @inheritParams vintalyse
#' @param date the latest performance month available
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
  date12 <-    as.Date(date) %m-% months(12)
  datedef <-   as.Date(date) %m-% months(default_definition)
  date13 <-    as.Date(date) %m-% months(13)
  datedef1 <-  as.Date(date) %m-% months(default_definition+1)
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

