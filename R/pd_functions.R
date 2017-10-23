
#' @title select_variables_pd_tr
#' @description selects the necessary variables from the standard data set for pd calculations
#' @inheritParams vintalyse
#' @param ... segmenter variables used for PD calculation
#'
#' @return the input data frame with only the necessary variables.
#' @export
#'
#' @examples
#' df_select <- select_variables_pd_tr(df, "fico_bin")
#' df_select <- select_variables_pd_tr(df, fico_bin)
select_variables_pd_tr <- function(data, ...) {
  data %>% select(contract_key, default_flag, pointintime_month, closing_balance, months_arrears, ...)
}


#' @title lead_flag
#' @description creates a lead variable for a flag based on the outcome period
#'   (or loss_id_period in the case of PD calculations)
#' @inheritParams vintalyse
#' @param outcome_period period to look ahead in terms of event (flag). E.g. for
#'   12 month PD this will be a loss identification period (LIP) of 12.
#' @param flag the flag or event (could be default, settlement, write-off or any other permanent status)
#'
#' @return the input data frame with a parameter called lead_flag
#' @export
#'
#' @examples
#' # creates lead flag for calculating PD:
#' dft <- lead_flag(df, 12, "default_flag")
#'
lead_flag <- function(data, outcome_period, flag) { ##########NOTE NAME AND PARAM CHANGE FOR FURTHER USE!!!!!

  flag <- string_to_quote(flag)

  data %>%
    group_by(contract_key) %>%
    mutate(lead_flag = lead(!!flag, outcome_period))
}


#F112Q1018643, F116Q2397770
