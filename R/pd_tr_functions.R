
# =============================================================================================================
# Project:        IAS39 impairment model
# Discription:    pd_tr_functions.R 
# Purpose:        Get data into the correct format to be used in the IAS39 provision engine
# Authors:        N van Heerden, DJ de Villiers
# Date:           17 Mar 2017
# =============================================================================================================


# libraries
# ---------
require(dplyr)
require(readr)
# ---------

# read in test data set
# ---------------------
# test_data <- read_rds("test_data.rds")
# ---------------------

# Utility functions 
# -----------------
# check that all closing balance is positive. 
pos_closing_balance <- function(data) {
  data %>% filter(closing_balance >= 0)  
}
# To use:
#df <- df %>% pos_closing_balance(.)

# function:: selects the necessary variables from the base set (df)
# in: the full data frame (df) containing all the standard variables. ... are all segementer variables used.  
select_variables_pd_tr <- function(data, ...) {
  data %>% select(contract_key, default_flag, pointintime_month, closing_balance, months_arrears, ...)  
}
# how to use:
#df <- select_variables_pd_tr(df, country, product_name)

# function::get_cohort_base
# calculates the total balance by arrears and date for each segmenter variables. 
# Essentially gets the sum(base) of your cohort. 
# ... are all segementer variables used
get_cohort_base <- function(data, default_definition, ...) {
  summarise(
    group_by(data, months_arrears, pointintime_month, ...), cohort_base = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}
# how to use:
#df_cohort_base <- get_cohort_base(df, default_definition = 4, country, product_name)

# function:: get_cohort_join. 
get_cohort_join <- function(dfx, dfy, segmenter_vector) {
  full_join(dfx, dfy, by = c("months_arrears", "pointintime_month", segmenter_vector))
}
# how to use: 
#df_cohort_join <-  get_cohort_join(dfx = df_cohort_base, dfy = df_cohort_default, segmenter_vector)
# -----------------

# PD Functions 
# ------------

# function:: lead default flag 
# loss id period is key user input. as a side note: using 12 month outcome on
# the 0 bucket gives input to NPV calculator.
lead_default_flag <- function(data, loss_id_period) {
  data %>% mutate(lead_default_flag = lead(default_flag, loss_id_period))   
}
# how to use
#df <- lead_default_flag(df, 3)

# get_cohort_default
get_cohort_default <- function(data, default_definition, ...) {
  summarise(
    group_by(data[data$lead_default_flag == 1, ], months_arrears, pointintime_month, ...), cohort_default = sum(closing_balance)) %>%
    filter(months_arrears < default_definition)
}
# how to use: 
#df_cohort_default <- get_cohort_default(df, 4, country, product_name)

# last step:
# main function that computes the dataset needed for pd's at the level specified
# by the segmenter variables.
# add: this when using function to calc pd: mutate(pd = cohort_default/cohort_base)
get_df_pd <- function(data = df, segmenter_vector, default_definition = 4, loss_id_period = 12, ...) {
  data <- data %>% 
    arrange(., contract_key, pointintime_month) %>%
#    pos_closing_balance(.) %>%
    select_variables_pd_tr(., ...) %>%
    lead_default_flag(., loss_id_period)
  
  df_cohort_base <- get_cohort_base(data = data, default_definition, ...)
  df_cohort_default <- get_cohort_default(data = data, default_definition, ...)
  df_cohort_join <-  get_cohort_join(dfx = df_cohort_base, dfy = df_cohort_default, segmenter_vector)
  
  return(df_cohort_join)
}
# ------------

# Transition rate functions
# -------------------------
# function that leads the arrears. It calculates arrears for different emergence periods. 
# will in future need to be based on the default definition. 
# default definition of 4 assumed in this case.
# This function can handle 1:3 months emergence. 
lead_arrears <- function(data, emergence_period) {
  data <- data %>%
    mutate(lead_arrears_1 = lead(months_arrears, 1)) %>%
    mutate(lead_arrears_2 = lead(months_arrears, 2)) %>%
    mutate(lead_arrears_3 = lead(months_arrears, 3))  
    
  if(emergence_period < 3) {
      data <- data %>% select(everything(), -num_range("lead_arrears_", (emergence_period+1):3))  
  } 
  else {data <- data}
}

# for tr use default definition 1 as we are only interested in the event where 
# contracts move forward out of 0 in arrears. thus any move out of 0 in arrears 
# can by seen as a default in this case. will always be one regardless of what
# the emergence period is.
# gets the arrears status after 1, 2 and 3 months. 
# works only for 1 months. This function not yet ready for 2:3 months. 
get_cohort_arrears <- function(data, emergence_period, ...) {
  if (emergence_period == 1) {
    return(
    summarise(
      group_by(data, months_arrears, lead_arrears_1, pointintime_month, ...), # works only for 1
      cohort_arrears = sum(closing_balance)
    ) %>%
      filter(months_arrears < 1) 
    )
  }  
  if (emergence_period == 3) {
    return(
      summarise(
        group_by(data, months_arrears, lead_arrears_3, pointintime_month, ...), # works only for 1
        cohort_arrears = sum(closing_balance)
      ) %>%
        filter(months_arrears < 1) 
    )
  } 
}

# get_cohort_arrears
# function for calculating transition rates into tr_x. 
# tr_x = roll from 0 to x in arrears. 
get_df_tr0_x <- function(data, emergence_period, tr_x, segmenter_vector, ...) {
  data <- select_variables_pd_tr(data, -default_flag, ...) %>%
    arrange(., contract_key, pointintime_month) %>%
    #pos_closing_balance(.) %>%
    lead_arrears(., emergence_period) 
  
  tr_cohort_base <- get_cohort_base(data, 1, ...)
  tr_cohort_arrears <- arrange(get_cohort_arrears(data, emergence_period, ...), by = pointintime_month)
  
  if (emergence_period == 1) {
    tr_join_df <- get_cohort_join(tr_cohort_arrears, tr_cohort_base, segmenter_vector) %>%
      filter(lead_arrears_1 == 1) %>%
      mutate(tr_0_1 = cohort_arrears/cohort_base)
  }

  if (emergence_period == 3) { 

    tr_join_df <- get_cohort_join(tr_cohort_arrears, tr_cohort_base, segmenter_vector) %>%
      filter(lead_arrears_3 == tr_x) %>%
      mutate(tr_0_x = cohort_arrears/cohort_base)
  }
  return(tr_join_df)
}

# get_df_tr
# main function that calculates the transition rates based on emergence period of either 1 or 3
# note that for 1 in arrears only the roll into 1 is calculated. 
get_df_tr <- function(data, emergence_period, segmenter_vector, ...) {
  if (emergence_period == 1) {
    df_tr_1 <- get_df_tr0_x(data, emergence_period, tr_x = 1, segmenter_vector, ...)
    return(df_tr_1)
  }
  
  if (emergence_period == 3) {
    df_tr_1 <- get_df_tr0_x(data, emergence_period = 3, tr_x = 1, segmenter_vector, ...)
    df_tr_2 <- get_df_tr0_x(data, emergence_period = 3, tr_x = 2, segmenter_vector, ...)
    df_tr_3 <- get_df_tr0_x(data, emergence_period = 3, tr_x = 3, segmenter_vector, ...)
    
    df_tr <- bind_rows(df_tr_1, df_tr_2, df_tr_3)
    return(df_tr)
  }
}
# use:
# get_df_tr(test_data, emergence_period = 3, segmenter_vector = c("country"), country)


# df1 <- select_variables_pd_tr(test_data, -default_flag, country) %>%
#   arrange(., contract_key, pointintime_month) %>%
#   pos_closing_balance(.) %>%
#   lead_arrears(., emergence_period = 3)
# 
# arr <- arrange(get_cohort_arrears(df1, 1, 3), pointintime_month)
# base <- get_cohort_base(df1, 1)
# 
# tr_join <- get_cohort_join(arr, base, c()) %>%
#   filter(lead_arrears_3 > 0)
# 
# tr_1 <- tr_join %>% 
#   filter(lead_arrears_3 == 1) %>%
#   mutate(tr_0_1 = cohort_arrears/cohort_base)
# 
#   tr_2 <- tr_join %>% 
#     filter(lead_arrears_3 == 2) %>%
#     mutate(tr_0_2 = cohort_arrears/cohort_base)
# 
#   tr_3 <- tr_join %>% 
#     filter(lead_arrears_3 == 3) %>%
#     mutate(tr_0_3 = cohort_arrears/cohort_base)
#   
# tr_join_df <- full_join(tr_1, tr_2, pointintime_month) %>%
#   full_join(., tr_3, pointintime_month)
  
# get_df_tr function for the specific case where emergence = 3
# get_df_tr3 <- function(data, emergence_period = 3, segmenter_vector, ...) {
#   data <- select_variables_pd_tr(data, -default_flag, ...) %>%
#     arrange(., contract_key, pointintime_month) %>%
#     pos_closing_balance(.) %>%
#     lead_arrears(., emergence_period) 
#   
#   tr_cohort_base <- get_cohort_base(data, 1, ...)
#   tr_cohort_arrears <- arrange(get_cohort_arrears(data, default_definition = 1, emergence_period = 1, ...), by = pointintime_month)
#   
#   tr_join_df <- get_cohort_join(tr_cohort_arrears, tr_cohort_base, segmenter_vector) %>%
#     filter(lead_arrears_1 == 1) %>%
#     mutate(tr_0_1 = cohort_arrears/cohort_base)
#   
#   return(tr_join_df)
# }

# -------------------------






# writing the test data set
# -------------------------
# df1 <- df %>%
#   filter(contract_key == 66 + 100000 | contract_key == 10111 + 100000 | contract_key == 21000 + 100000 |contract_key == 37024 + 100000 | 
#            contract_key == 37025 + 100000 | contract_key == 8302 + 100000 | contract_key ==  129805)
# 
# write_rds(df1, "test_data.rds")
# -------------------------



