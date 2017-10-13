# library(ggplot2)

# need to put this into data wrangling
# df <- df %>% filter(pointintime_month < "2017-01-01")

#' @title vintalyse
#' @description produces
#'
#' @param data a monthly loan performance level data frame in standard
#'   \href{https://github.com/TheProfitTable/masterlibrary/blob/master/tpt_credit_datadictionary.Rmd}{data
#'    dictionary} format
#' @param period_dim the name of the period dimension used to do the vintage
#'   analysis. Usually either "loan_period" or "fpd_period". Note, must be input
#'   as a "string".
#' @param month_dim the name of the month dimension used to do the vintage
#'   analysis. Usually either "orig_month" or "fpd_month". Note, must be input
#'   as a "string".
#' @param nosegment TRUE or FALSE. Default = TRUE. When TRUE then no
#'   segmentation will be done when doing vintage analysis. If FALSE, then
#'   segmentation will be done according to the variable entered as var. If
#'   FALSE, then a value for var must be provided.
#' @param var the variable to segment the vintage analysis by. Must be provided
#'   as "string" and must be a categorical variable. See Note below.
#'
#' @return A data set used for plotting default vintage analysis.
#' @export
#' @note Each level of the var pararmeter needs to have sufficient volume to
#'   produce meaningful results. When creating a plot for the vintage analysis,
#'   the var parameter needs to be either filtered upon or used to create a
#'   grid.
#' @examples
#' default_summary <- vintalyse(df, "loan_period", "orig_month")
#' default_summary_var <- vintalyse(df, "loan_period", "orig_month", nosegment = FALSE, var = "fico_bin")
vintalyse <- function(data, period_dim = "loan_period", month_dim = "orig_month", nosegment = TRUE, var)  {

  period_dim <- as.name(eval(period_dim))
  month_dim <- as.name(eval(month_dim))

  period_dim <- enquo(period_dim)
  month_dim <- enquo(month_dim)

  if (nosegment) {

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

  } else {

    var <- as.name(eval(var))
    var <- enquo(var)

    default_summary <- df %>%
      group_by(!!period_dim, !!month_dim, !!var) %>% # Define the grouping variables
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
  }

  return(default_summary)

}

# early default function
# -----------------------

# default_definition = 3
# # build flags
# # use either loan or fpd period to filter and build flags
# df_arrflags <- df %>%
#   filter(fpd_period <= default_definition)
#
# for (i in 1:default_definition) {
#   df_arrflags <- df_arrflags %>%
#       mutate(!!paste0("arr_flag_", i) := if_else(fpd_period == i & months_arrears == i, true = 1, false = 0))
# }
#
# df_arrflags_sum <- df_arrflags %>%
#   group_by(orig_month) %>%
#   summarise(arr_flag_1 = sum())


# By orig_month: arr1 = sum(flag1) / sum(all)
# -----------------------




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
