
#' @title select_variables_pd
#' @description selects the necessary variables from the standard data set for
#'   pd calculations
#' @inheritParams vintalyse
#' @param segmenter_vector segmenter variables used for PD calculation. A vector
#'   of variable names as strings.
#'
#' @return the input data frame with only the necessary variables.
#' @export
#'
#' @examples
#' df_select <- select_variables_pd_tr(df, c("fico_bin"), 2)
#' df_select <- select_variables_pd_tr(df,  1)
select_variables_pd <- function(data, segmenter_level = 1, segmenter_vector) {

  select_vector <- switch(segmenter_level,
         c("contract_key", "default_flag", "pointintime_month", "closing_balance", "months_arrears"),
         c("contract_key", "default_flag", "pointintime_month", "closing_balance", "months_arrears", segmenter_vector),
         c("contract_key", "default_flag", "pointintime_month", "closing_balance", "months_arrears", segmenter_vector))

  data %>% select(select_vector)
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
#' @param group_by_vector Vector with string names of variables to be grouped
#'   by. Created in pd function.
#'
#' @return A data frame containing the sum of closing balance by
#'   pointintime_month, months_arrears smaller than the default definition, and
#'   the segmenter variables used.
#' @export
#' @note The cohort_base values essentially form the 'base' when doing PD
#'   calculations. cohort_base and cohort_default form the
#' @examples
#' get_cohort_base(df, 3, c("months_arrears", "pointintime_month"))
#'
get_cohort_base <- function(data, default_definition, group_by_vector) {
  summarise(
    group_by_at(data, group_by_vector), cohort_base = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}

# group_by_vector to be in pd: c("months_arrears", "pointintime_month") or c("months_arrears", "pointintime_month", segmenter_vector)

#' @describeIn get_cohort_base Calculates the total balance by arrears and date for each
#'   segmenter variables where the event (e.g. default) has occured.
get_cohort_default <- function(data, default_definition, group_by_vector) {

  summarise(
    group_by_at(data[data$lead_flag == 1, ], group_by_vector), cohort_default = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}



#' @title get_cohort_join
#' @description joins the sets created by \code{\link{get_cohort_default}} and \code{\link{get_cohort_base}}
#' @param dfx data frame created by get_cohort_base
#' @param dfy data frame created by get_cohort_default
#' @inheritParams get_cohort_base
#'
#' @return joined data set with both the summarised base values and event (e.g. default) values.
#' @export
#'
#' @examples
#' df_cohort_join <-  get_cohort_join(dfx = df_cohort_base, dfy = df_cohort_default,  group_by_vector)
#'
get_cohort_join <- function(dfx, dfy, group_by_vector) {
  full_join(dfx, dfy, by = group_by_vector)
}



#' pd
#' @title Probability of Default
#' @description Calculates the probability of default for various arrears
#'   buckets
#' @inheritParams vintalyse
#' @param default_definition default definition (in months past due) applicable
#'   to the loan portfolio. The pd for all arrears buckets smaller than this
#'   will be calculated. I.e. if you only want the PD for the 0 in arrears
#'   bucket then make this 1.
#' @param loss_id_period The number of months to look ahead when calculating the
#'   PD.
#'
#' @return A data frame containing the PD plus the total sum of balance for the
#'   total as well as defaulters. This is done by arrears bucket,
#'   pointintime_month, and the variables specified in var1 and var2.
#' @export
#'
#' @examples
#' df_pd <- pd(df, 4, 12, 2, "fico_bin")
#' # 12 month PD's for 0 in arrears only. no segmentation:
#' df_pd <- pd(df, 1, 12, 1)
#'
pd <- function(data = df, default_definition, loss_id_period = 12, segmenter_level = 1, var1, var2) {

  segmenter_vector <- switch(segmenter_level,
                             c(NULL),
                             c(var1),
                             c(var1, var2))

  group_by_vector <- switch(segmenter_level,
                            c("months_arrears", "pointintime_month"),
                            c("months_arrears", "pointintime_month", segmenter_vector),
                            c("months_arrears", "pointintime_month", segmenter_vector))

  data <- data %>%
    arrange(., contract_key, pointintime_month) %>%
    #    pos_closing_balance(.) %>%
    select_variables_pd_tr(., segmenter_level,segmenter_vector) %>%
    lead_flag(., outcome_period = loss_id_period, "default_flag")

  df_cohort_base <- get_cohort_base(data = data, default_definition, group_by_vector)
  df_cohort_default <- get_cohort_default(data = data, default_definition, group_by_vector)

  df_cohort_join <-  get_cohort_join(dfx = df_cohort_base, dfy = df_cohort_default, group_by_vector) %>%
    mutate(probability_of_default = cohort_default/cohort_base)

  return(df_cohort_join)
}


#F112Q1018643, F116Q2397770
