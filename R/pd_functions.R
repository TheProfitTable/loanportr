
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
lead_flag <- function(data, outcome_period, flag) {

  flag <- string_to_quote(flag)

  data %>%
    group_by(contract_key) %>%
    mutate(lead_flag = lead(!!flag, outcome_period))
}


#' @title get_cohort_base
#' @description Calculates the total balance by arrears and date for each
#'   segmenter variables.
#' @inheritParams early_default
#' @param ... Segmenter variables. Can be more than one.
#'
#' @return A data frame containing the sum of closing balance by
#'   pointintime_month, months_arrears smaller than the default definition, and
#'   the segmenter variables used.
#' @export
#' @note The cohort_base values essentially form the 'base' when
#'   doing PD calculations. cohort_base and cohort_default form the
#' @examples
#' # nested fico score and occupancy status segmentation
#' df_cohort_base <- get_cohort_base(df, default_definition = 1, fico_bin, occpy_sts)
#' # only one segmentation: fico score
#' df_cohort_base <- get_cohort_base(df, default_definition = 3, fico_bin)
#' # how to use:
#' df_cohort_default <- df %>% lead_flag(., 12, "default_flag") %>%
#'   get_cohort_default(., 3, fico_bin)
#'
get_cohort_base <- function(data, default_definition, ...) {
  summarise(
    group_by_at(data, months_arrears, pointintime_month, ...), cohort_base = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}


#' @describeIn get_cohort_base Calculates the total balance by arrears and date for each
#'   segmenter variables where the event (e.g. default) has occured.
get_cohort_default <- function(data, default_definition, ...) {

  summarise(
    group_by(data[data$lead_flag == 1, ], months_arrears, pointintime_month, ...), cohort_default = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}



#' @title get_cohort_join
#' @description joins the sets created by \code{\link{get_cohort_default}} and \code{\link{get_cohort_base}}
#' @param dfx data frame created by get_cohort_base
#' @param dfy data frame created by get_cohort_default
#' @param segmenter_vector a vector of strings that contain the segmenter variable names.
#'
#' @return joined data set with both the summarised base values and event (e.g. default) values.
#' @export
#'
#' @examples
#' df_cohort_join <-  get_cohort_join(dfx = df_cohort_base, dfy = df_cohort_default, c("fico_bin"))
#'
get_cohort_join <- function(dfx, dfy, segmenter_vector) {
  full_join(dfx, dfy, by = c("months_arrears", "pointintime_month", segmenter_vector))
}
# how to use:

# NOTE: make ... input as strings for all variables.


#F112Q1018643, F116Q2397770
