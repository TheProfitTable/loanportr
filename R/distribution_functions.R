

# packages
# --------
# library(dplyr)
# library(tidyr)
# library(rlang)
# --------

# import data
# ------------
# library(readr)
# df <- read_rds("df.rds")
# ------------

# exmamples of what I want to do.
# distribution of fico_bins by loan_amount over orig_month

#' @title make_orig
#' @description condences monthly performance data set to origination level
#' @param data a monthly loan performance level data frame in standard data
#'   dictionary format
#' @param use_period either TRUE or FALSE. Default is FALSE, uses
#'   pointintime_month == orig_month to filter. If TRUE then uses period and
#'   period_number specified to filter.
#' @param period only specified if use_period = TRUE. Either fpd_period or
#'   loan_period. used in conjunction with period_number to filter to unique
#'   contract level.
#' @param period_number the loan_period or fpd_period to use when filtering.
#'   usually 1.
#'
#' @return a loan level data set (should be unique on contract_key). for use in
#'   \code{\link{vardistr_amnt}} and \code{\link{vardistr_amnt}}
#'   vardistr_amnt functions.
#' @export
#'
#' @examples
#'
#' df_orig_test <- make_orig(df)
#' df_orig_test <- make_orig(df, use_period = TRUE, period = loan_period, period_number = 1)

make_orig <- function(data, use_period = FALSE, period = loan_period, period_number = 1) {

  if(use_period) {
    period <- enquo(period)
    #period <- substitute(period)
    df_orig <- data %>%
      filter(UQ(period)  == period_number)

  } else {

    df_orig <- data %>%
      filter(orig_month == pointintime_month)

  }
  return(df_orig)
}


#' @describeIn vardistr_perc This only returns the amounts in each var level and the Total
vardistr_amnt <- function(data, var, weight, datedim, ...) {
  # could convert this fun for use in on book by adding if else here that also
  # assigns data directly to df_sales_vintage and use datedim =
  # pointintime_month
  df_orig <- make_orig(data, ...)

  if (is.null(as.null(weight))) {
    weight <- as.null(weight)
  } else {
    weight <- as.name(eval(weight))
  }

  var <- as.name(eval(var))
  datedim <- as.name(eval(datedim))

  var <- enquo(var)
  datedim <- enquo(datedim)
  weight <- enquo(weight)

  df_sales_vintage <- df_orig %>%
    count(., !!var, !!datedim, wt = !!weight) %>%
    filter(!is.na(!!var)) %>%
    spread(!!var, n)

  ncol <- ncol(df_sales_vintage)
  #names <- names(df_sales_vintage)

  df_sales_vintage$total <- apply(X = df_sales_vintage[,c(2:ncol)], MARGIN =  1, FUN = sum)

  return(df_sales_vintage)
}

# ------------------------------

#' @title vardistr_perc
#' @description turns loan level data to monthly distributions
#' @param data a monthly loan performance level data frame in standard
#'   \href{https://github.com/TheProfitTable/masterlibrary/blob/master/tpt_credit_datadictionary.Rmd}{data
#'    dictionary}  format
#' @param var the variable by which you wish to show the monthly distribution.
#'   Must be a "string".
#' @param weight either "NULL" or a weight variable such as "loan_amount" or
#'   "closingbalance". When "NULL" the weight is count of contracts. Note, must
#'   be entered as a "string".
#' @param datedim the date field by which you wish to show the distribution.
#'   Usually "orig_month" or "fpd_month". "pointintime_month" coming soon. Must be entered as a "string".
#' @param ... parameters used in \code{\link{make_orig}} function.
#'
#' @return a data frame with unique datedim in first column. other columns are
#'   the various levels of the var parameter. There is also a Total variable
#'   that contains the total of the weight parameter.
#' @export
#'
#' @examples
#' df_vdp_test <- vardistr_perc(df, "fico_bin", "loan_amount", "orig_month", use_period = TRUE, period = fpd_period)
#' df_vdp_test <- vardistr_perc(df, "fico_bin", "NULL", "fpd_month")
#'
#' df_vda <- vardistr_amnt(df, "fico_bin", "loan_amount", "orig_month", use_period = TRUE, period=fpd_period)
#' df_vda <- vardistr_amnt(df, "fico_bin", "loan_amount", "orig_month")
vardistr_perc <- function(data, var, weight, datedim, ...) {

  #var <- enquo(var)
  #datedim <- enquo(datedim)
  #weight <- enquo(weight)

  df_vda <- vardistr_amnt(data, var, weight, datedim, ...)

  ncol <- ncol(df_vda) - 1

  for (i in 2:ncol) {
    name  <- as.name(names(df_vda[i])) # as name
    nname <- paste0("perc_", names(df_vda[i])) # as string

    total <- quo(total)

    df_vda <- mutate(df_vda, !!nname := UQ(name) / UQ(total))
  }
  return(df_vda)
}




