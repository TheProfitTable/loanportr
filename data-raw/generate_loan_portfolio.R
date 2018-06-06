
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
# generate test dataset for loanportr package:


# Step 1: Generate contract-level loans

set.seed(111) # for reproducible result

portfolio_start_date <- "2017-11-15" # any randon date(month) that the loan portfolio should start
portfolio_period <- 12 # the length of the entire portfolio
number_of_contracts_per_month <- 3 # number of contracts that originate each month (should probably be more of a normal distribution around a set number)

contract_key_vector <- c(100000:200000) # random contract key number can be between 100000 and 200000
term_vector <- c(12,24,36,48,60) # 1 to 5 year terms
loan_amount_vector <- seq(from = 10000, to = 100000, by = 10000) # loan amounts

# generate a semi random perfect loan based on above vectors
#-------------------
#' @title Generate a semi random perfect loan
#'
#' @param start_month A date string to start the loan, such as "2010-08-24".
#'
#' @return a dataframe of a single semi random generated loan
#' @export
#'
#' @examples loan <- generateLoan("2010-08-24")
generateLoan <- function(start_month)
{

  loan <- data_frame(
    contract_key = sample(contract_key_vector, 1, replace=TRUE), # sample random contract key from vector
    term = sample(term_vector, size = 1), # sample random term from vector
    orig_date = sample(seq(floor_date(as.Date(start_month), "month"), ceiling_date(as.Date(start_month), "month"), by="day"), 1), # sample a random day within the month of "start_month"
    loan_amount = sample(loan_amount_vector, 1, replace=TRUE) # sample random loan amount from vector
  )
  loan <- loan %>% slice(rep(1:n(), times = term)) # repeat row for duration of term
  loan_period <- 1:loan$term[1]
  pointintime_month <- seq(ceiling_date(as.Date(start_month), "month"), by="1 month", length=loan$term[1]) - days(1) # increment monthly
  loan <- cbind(loan, loan_period, pointintime_month) # add columns "loan_period" and "pointintime_month"
  return(loan)
}


# generate a portfolio of contracts based on number_of_contracts_per_month with unique contract_keys from vector contract_key_vector
#-----------------------------
#' @title Generate a portfolio of loan contracts
#'
#' @param portfolio_start_date A date (month) string to start the portfolio, such as "2010-08-24".
#' @param portfolio_period A numeric period in month the portfolio should cover, such as 12 for a year
#' @param number_of_contracts_per_month A numeric value for the number of loan originations there should be per month
#'
#' @return a loan portfolio dataframe based on the input parameters
#' @export
#'
#' @examples p_loans <- generatePortfolio(portfolio_start_date = "2010-05-10", portfolio_period = 12, number_of_contracts_per_month = 100)
#'
generatePortfolio <- function(portfolio_start_date = portfolio_start_date,
                              portfolio_period = portfolio_period,
                              number_of_contracts_per_month = number_of_contracts_per_month){

  portfolio_period_vector <- seq(ceiling_date(as.Date(portfolio_start_date), "month"), by="1 month", length=portfolio_period) - days(1)

  portfolio_loans <- data_frame()
  for (month_of_interest in as.list(portfolio_period_vector)){

    # generate a random set of loans for the specific month based on the number_of_contracts_per_month
    loans_for_month <- data_frame()
    for(i in 1:number_of_contracts_per_month){
      loan <- generateLoan(month_of_interest)
      loans_for_month <- rbind(loans_for_month, loan)
    }

    portfolio_loans <- rbind(portfolio_loans, loans_for_month)

  }
  return(portfolio_loans)
}



# Step 2: Generate monthly-level data on loans

# Step 3: Introduce defaults and other naunces into generated loan portfolio
