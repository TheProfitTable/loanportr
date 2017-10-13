# library(ggplot2)

# need to put this into data wrangling
# df <- df %>% filter(pointintime_month < "2017-01-01")

#' @title vintalyse
#' @description produces
#'
#' @param data a monthly loan performance level data frame in standard
#'   \href{https://github.com/TheProfitTable/masterlibrary/blob/master/tpt_credit_datadictionary.Rmd}{data
#'   dictionary} format
#' @param period_dim the name of the period dimension used to do the vintage analysis. Usually either
#' "loan_period" or "fpd_period". Note, must be input as a "string".
#' @param month_dim the name of the month dimension used to do the vintage analysis. Usually either
#' "orig_month" or "fpd_month". Note, must be input as a "string".
#'
#' @return A data set used for plotting default vintage analysis.
#' @export
#' @note to be generalised to work for any flag. not just default.
#' @examples
#' default_summary <- vintalyse(df, "loan_period", "orig_month")
#'
vintalyse <- function(data, period_dim = "loan_period", month_dim = "orig_month")  {

  period_dim <- as.name(eval(period_dim))
  month_dim <- as.name(eval(month_dim))

  period_dim <- enquo(period_dim)
  month_dim <- enquo(month_dim)

  default_summary <- df %>%
    group_by(!!period_dim, !!month_dim) %>% # Define the grouping variables
    summarise( # Now you define your summary variables with a name and a function...
      total_count = n(),  # The function n() in dlpyr gives you the number of observations
      default_count = sum(default_flag ,na.rm = TRUE),
      default_perc = default_count / total_count,
      total_loan_amount = sum(loan_amount, na.rm = TRUE),
      default_loan_amount = sum(default_flag*loan_amount, na.rm = TRUE),
      default_perc_loan_amount = default_loan_amount / total_loan_amount,
      total_closing_balance =  sum(closing_balance, na.rm = TRUE),
      default_closing_balance = sum(default_flag*closing_balance, na.rm = TRUE),
      default_perc_closing_balance = default_closing_balance / total_closing_balance
    )
  return(default_summary)
}






# arrange to make viewing easier.

# default_summary <- default_summary %>% arrange(orig_month, loan_period) %>%
#   filter(orig_month < "2013-01-01")

# data <- ggplot(data=default_summary,
#                aes(x=loan_period,
#                    y=default_perc_loan_amount,
#                    group=orig_month,
#                    colour=orig_month,
#                    xlab = "Loan Period (months)", ylab = "NPL Percentage (%)")) +
#   geom_line(aes(alpha=0.8)) +
#   geom_point()
#
# plot(data)
