
# var
# in: string
# used in:
#   group_by with date variables (sometimes dynamic) --> can be done with group_by_at and stringed vector
#   to determine no segmentation vs segmentation, --> new variable: segmentation levels?
#   to join (stringed vector), --> only need stringed vector
# how?
# put together stringed vector inside function: group_by_vector or join_vector




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
#' @param segmenter_level 1, 2 or 3. Default = 1. When 1 then no
#'   segmentation will be done when doing vintage analysis. If 2, then
#'   segmentation will be done by to the variable entered as var1. If
#'   3, then segmentation will be done by var1 and var2.
#' @param var1 the main variable to segment the vintage analysis by. Must be provided
#'   as "string" and must be a categorical variable. See Note below.
#' @param var2 if a second level of segmentation is required, this is the second level.
#'
#' @return A data set used for plotting default vintage analysis.
#' @export
#' @note Each level of the var1 and var2 pararmeters need to have sufficient volume to
#'   produce meaningful results. When creating a plot for the vintage analysis,
#'   the var1 and var2 parameters need to be either filtered upon or used to create a
#'   grid.
#' @examples
#' default_summary <- vintalyse(df, "loan_period", "orig_month")
#' default_summary_var1 <- vintalyse(df, "loan_period", "orig_month", segmenter_level = 3, var1 = "fico_bin")
#' default_summary_var2 <- vintalyse(df, "loan_period", "orig_month", segmenter_level = 3, var1 = "fico_bin", "occpy_sts")
vintalyse <- function(data, period_dim = "loan_period", month_dim = "orig_month", segmenter_level = 1, var1, var2)  {

  # period_dim <- as.name(eval(period_dim))
  # month_dim <- as.name(eval(month_dim))
  #
  # period_dim <- enquo(period_dim)
  # month_dim <- enquo(month_dim)

  group_by_vector <- switch(segmenter_level,
                            c(period_dim, month_dim),
                            c(period_dim, month_dim, var1),
                            c(period_dim, month_dim, var1, var2))


    default_summary <- df %>%
      group_by_at(group_by_vector) %>% # Define the grouping variables
      summarise( # Now you define your summary variables with a name and a function...
        total_count = n(),  # The function n() in dlpyr gives you the number of observations
        default_count = sum(default_flag ,na.rm = TRUE),
        default_perc_count = default_count / total_count,
        total_loan_amount = sum(loan_amount, na.rm = TRUE),
        default_loan_amount = sum(default_flag*loan_amount, na.rm = TRUE),
        default_perc_loan_amount = default_loan_amount / total_loan_amount,
        total_closing_balance =  sum(closing_balance, na.rm = TRUE),
        default_closing_balance = sum(default_flag*closing_balance, na.rm = TRUE),
        default_perc_closing_balance = default_closing_balance / total_closing_balance
      )

#
#     var <- as.name(eval(var))
#     var <- enquo(var)

  return(default_summary)

}

# early default function
# -----------------------

#' @title early_default
#' @description Calculates early arrears rolls by first pay date vintage. I.e. %
#'   loans in arrears bucket 1 at month 1 and arrears 2 in month 2 etc.
#' @param data a monthly loan performance level data frame in standard
#'   \href{https://github.com/TheProfitTable/masterlibrary/blob/master/tpt_credit_datadictionary.Rmd}{data
#'    dictionary} format
#' @param default_definition the default definition applied to the loan
#'   portfolio
#' @param var variable to segment the early arrears rates by. Must be a
#'   "string". If = "NULL" there will be no segmentation.
#'
#' @return a data frame consisting of variables that represent the rate of roll
#'   straight from origination through the various arrears buckets into default.
#'   I.e. if the default definition is 3 then % of contracts that are in default
#'   in month 3 (period since first pay date - fpd_period). As well as the %
#'   loans in arrears 1 in month 1, 2 in month 2, etc. up to default.
#' @note the rate is calculated based on the following weights: loan_amount,
#'   count, closing_balance. A variable for each is included in the data frame.
#' @export
#'
#' @examples
#' df_arrflags_test <- early_default(df, default_definition = 3, var = "NULL")
#' df_arrflags_test <- early_default(df, default_definition = 3, var = "fico_bin")
#'
early_default <- function(data, default_definition, var) {
    # no segmentation
  if (is_null_string(var)) {

    df_arrflags <- data %>%
      filter(fpd_period <= default_definition)

    # create early arrears flags
    for (i in 1:default_definition) {
      df_arrflags <- df_arrflags %>%
        mutate(!!paste0("arr_flag_", i) := if_else(fpd_period == i & months_arrears == i, true = 1, false = 0))
    }

    # create table with dates to join to9
    df_arrflags_all <- df_arrflags %>% group_by(fpd_month) %>% summarise() #test

    for (i in 1:default_definition) {
      name <- as.name((paste0("arr_flag_", i)))

      # must repeat this section in loop:
      df_arrflags_sum <- df_arrflags %>%
        filter(fpd_period == i) %>%
        group_by(fpd_month) %>% #var
        summarise(!!paste0("arr_flag_", i, "_count") := sum(UQ(name)) / n(),
                  !!paste0("arr_flag_", i, "_loan_amount") := sum(UQ(name)*loan_amount) / sum(loan_amount),
                  !!paste0("arr_flag_", i, "_closing_balance") := sum(UQ(name)*closing_balance) / sum(closing_balance))

      df_arrflags_all <- inner_join(x = df_arrflags_sum, y = df_arrflags_all, by = c("fpd_month")) #var
    }

    #segmantation
  } else {
    by_var <- c("fpd_month", var)
    var <- as.name(eval(var))
    var <- enquo(var)

    df_arrflags <- data %>%
      filter(fpd_period <= default_definition)

    # create early arrears flags
    for (i in 1:default_definition) {
      df_arrflags <- df_arrflags %>%
        mutate(!!paste0("arr_flag_", i) := if_else(fpd_period == i & months_arrears == i, true = 1, false = 0))
    }

    # create table with dates to join to9
    df_arrflags_all <- df_arrflags %>% group_by(fpd_month, !!var) %>% summarise() #test

    for (i in 1:default_definition) {
      name <- as.name((paste0("arr_flag_", i)))

      # must repeat this section in loop:
      df_arrflags_sum <- df_arrflags %>%
        filter(fpd_period == i) %>%
        group_by(fpd_month, !!var) %>% #var
        summarise(!!paste0("arr_flag_", i, "_count") := sum(UQ(name)) / n(),
                  !!paste0("arr_flag_", i, "_loan_amount") := sum(UQ(name)*loan_amount) / sum(loan_amount),
                  !!paste0("arr_flag_", i, "_closing_balance") := sum(UQ(name)*closing_balance) / sum(closing_balance))

      df_arrflags_all <- inner_join(x = df_arrflags_sum, y = df_arrflags_all, by = by_var) #var
    }
  }
  return(df_arrflags_all)
}

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
