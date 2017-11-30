

# packages
# --------
# library(dplyr)
# library(tidyr)
# library(rlang)
# library(lubridate)
# --------

# import data
# ------------
# library(readr)
# df <- read_rds("df_fredmac.rds")
# df <- read_rds("df.rds")
# ------------

#' make_orig
#' @title Make the loan data frame an origination level one
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
vardistr_amnt <- function(data, var1, var2, weight, datedim, segmenter_level = 2, ...) {

  if (datedim == "pointintime_month") {
    df_orig <- data
  } else {
    df_orig <- make_orig(data, ...)
  }

  if (is_null_string(weight)) {
    weight <- as.null(weight)
  } else {
    weight <- as.name(eval(weight))
  }

  # var <- as.name(eval(var))

  datedim <- string_to_quote(datedim)
  var1 <- string_to_quote(var1)
  # var <- enquo(var)

  weight <- enquo(weight)

  if (segmenter_level == 3) {
    var2 <- string_to_quote(var2)
    df_sales_vintage <- df_orig %>%
      count(., !!var1, !!var2, !!datedim, wt = !!weight) %>%
      filter(!is.na(!!var1)) %>%
      filter(!is.na(!!var2)) %>%
      spread(!!var2, n) # will need to filter on var1 when using output
    # ncol <- ncol(df_sales_vintage)
    # df_sales_vintage$total <- apply(X = df_sales_vintage[,c(3:ncol)], MARGIN =  1, FUN = sum, na.rm = TRUE)
  } else {
    df_sales_vintage <- df_orig %>%
      count(., !!var1, !!datedim, wt = !!weight) %>%
      filter(!is.na(!!var1)) %>%
      spread(!!var1, n)

  }

  ncol <- ncol(df_sales_vintage)
  df_sales_vintage$total <- apply(X = df_sales_vintage[,c(segmenter_level:ncol)], MARGIN =  1, FUN = sum, na.rm = TRUE)

  return(df_sales_vintage)
}

# ------------------------------

#' vardistr_perc
#' @title Variable distribution over time
#' @description turns loan level data to monthly distributions
#' @param data a monthly loan performance level data frame in standard
#'   \href{https://github.com/TheProfitTable/masterlibrary/blob/master/tpt_credit_datadictionary.Rmd}{data
#'    dictionary}  format
#' @param var1 the main (first) variable by which you wish to show the monthly
#'   distribution. Must be a "string". When segmenter_level = 2 then the levels
#'   of this variable are spread across the data frame. If segmenter_variable is
#'   = 3 then the unique levels of this variable are in the first column of the
#'   resultant data frame.
#' @param var2 Only used when segmenter_level = 3. The unique levels of this
#'   variable are then spread across the resultant data frame.
#' @param weight either "NULL" or a weight variable such as "loan_amount" or
#'   "closingbalance". When "NULL" the weight is count of contracts. Note, must
#'   be entered as a "string".
#' @param datedim the date field by which you wish to show the distribution.
#'   Usually "orig_month" or "fpd_month". "pointintime_month" is used when point
#'   in time distributions want to be done over time (book analysis). Must be
#'   entered as a "string".
#' @param segmenter_level Default is = 2. Then only one level of segmentation
#'   (distribution over time) is done. If = 3 then two levels of segmentation
#'   are done, var1 being the main level and var 2 the sub-level.
#' @param ... parameters used in \code{\link{make_orig}} function.
#'
#' @return a data frame with unique datedim in first column. other columns are
#'   the various levels of the var parameter. There is also a Total variable
#'   that contains the total of the weight parameter.
#' @export
#'
#' @examples
#' f_vdp_test <- vardistr_perc(data = df, var1 = "country", weight = "net_advance",
#' datedim = "pointintime_month", segmenter_level = 2, use_period = TRUE, period = fpd_period)
#'
#' x <- vardistr_perc(data = df, var1 = "disclosure",
#' weight = "net_advance", datedim = "pointintime_month", segmenter_level = 2)
#'
vardistr_perc <- function(data, var1, var2, weight, datedim, segmenter_level = 2, ...) {

  #var <- enquo(var)
  #datedim <- enquo(datedim)
  #weight <- enquo(weight)

  df_vda <- vardistr_amnt(data, var1, var2, weight, datedim, segmenter_level, ...)

  return(add_perc(df_vda, segmenter_level))
}













