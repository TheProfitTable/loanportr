
# =============================================================================================================
# Project:        IAS39 impairment model
# Discription:    lgd_functions.R 
# Purpose:        Calculate LGD1 curves to be used in the IAS39 provision engine
# Authors:        N van Heerden, DJ de Villiers
# Date:           31 Mar 2017
# =============================================================================================================

# Note:
# Code optimized from the work performed by Erika Slabber, 16 March 2017
# Did not use run-off factors (enough data points?)

# libraries
# ---------
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(data.table)
# ---------

# functions
# =========
#' @title npv_cf_calculation
#' discounts a stream of cash flows
#' @author NvH
#' @param cf_length The number of cashflows in the cashflow stream.
#' @param cf a matrix containing the cash flows. a row should contain an stream
#'   of cashflows.
#' @param discount_rate the annual discount rate used to discount the cashflows
#' @param rownumber the rownumber in the matrix that contains the cash flows to
#'   be discounted. the default is 1.
#' @param times_pa the times per annum that a cashflow is received. E.g. is cash
#'   flows are monthly, times_pa = 12. If they are annual times_pa = 1. the
#'   default is 12.
#'   
#' @return The function returns 1 x cf_length matrix containing the discounted value for each cash flow
#' @export
#' 
#' @examples
#' # general
#' x <- c(1:15)
#' x <- matrix(x,nrow = 1,ncol = 15)
#' npv_cf_calculation(cf_length = 15, cf = x, discount_rate = 0.1)
#' # for use in lgd_functions.R
#' NPV_calculation_K <- npv_cf_calculation(cf_length =  prediction_period, cf = final_recoveries, 
#' discount_rate = discount_rate_k)
#' 
npv_cf_calculation <- function(cf_length, cf, discount_rate, rownumber = 1, times_pa = 12) {
  sapply(1:(cf_length),
         function(j) {
           cf[rownumber,j]*(1+(discount_rate/times_pa))^(-j)
         }
  )
}

#' @title calc_lgd_from_recoveries
#'   
#' @param cf_length The number of cashflows in the cashflow stream.
#' @param cf a matrix containing the cash flows. a row should contain an stream 
#'   of cashflows. for this function the cf input needs to be a recovery %
#'   calculted for lgd purposes.
#' @param discount_rate the annual discount rate used to discount the cashflows
#' @param rownumber the rownumber in the matrix that contains the cash flows to 
#'   be discounted. the default is 1.
#' @param times_pa the times per annum that a cashflow is received. E.g. is cash
#'   flows are monthly, times_pa = 12. If they are annual times_pa = 1. the 
#'   default is 12.
#'   
#' @return
#' @export
#' @seealso npv_cf_calculation()
#'   
#' @examples
#' calc_lgd_from_recoveries(cf_length = 60, cf = final_recoveries, discount_rate = discount_rate_u, 
#' rownumber = 1, times_pa = 12)
#' calc_lgd_from_recoveries(cf_length = 60, cf = lgd_results$rr_cf[[2]], discount_rate = discount_rate_u, 
#' rownumber = 1, times_pa = 12)
#' 
calc_lgd_from_recoveries <- function(cf_length, cf, discount_rate, rownumber = 1, times_pa = 12) {
  npv <- npv_cf_calculation(cf_length =  cf_length, cf = cf, 
                            discount_rate = discount_rate, rownumber = rownumber, times_pa = times_pa)
  
  return(1 - sum(npv))
}

# =========

# check if data already imported and assign 
if (!exists("df", mode = "list")) {
  df <- readRDS("df.rds")
} 



# make a copy to run both kenya and uganda
data <- df 

# check if LGD results were previously calculated
if(file.exists("lgd_results.rds")) {  
  lgd_results <- read_rds("lgd_results.rds")
}else {


# KENYA
# =====
####################inputs##############################

prediction_period <- 60   #Number of periods receipts are expected: GVR = 60
discount_rate_k <- 0.3344      #Rate used to discount receipts
#monthly_effective <- 0.2652534071  #what will be received as input? 
n <- 121                  #Number of vitages used in calculation of run-off factors

########################################################


#################data analysis##########################
#Sort data
data <- arrange(data, contract_key, pointintime_month) 

#only keep entries from default onwards(LGD modelling), receipting: 60 months
data<-data[!(data$default_flag==0) & (data$months_in_default <= prediction_period),]
#2 625 346obs, now: 1 627 940

#Set negative closing balances = 0
#See positive receipts as reversals
data$closing_balance[data$closing_balance < 0] <- 0

data <- data %>% filter(country == "Kenya")

#recalculate exposure_at_default

setDT(data)[, exposure_at_default2 := closing_balance[months_in_default == 1], by = contract_key]

#Summarize data
LGD_Summary<-data %>%
  group_by(default_month, months_in_default) %>%
  summarize(payment_sum= sum(payment, na.rm=TRUE), 
            closing_balance_sum = sum(closing_balance, na.rm = TRUE), 
            exposure_at_default_sum = sum(exposure_at_default2, na.rm = TRUE))

#calculate cumulative values and percentages
LGD_Summary<- LGD_Summary %>% group_by(default_month) %>% mutate(cumsum_payment = cumsum(payment_sum), cumsum_balance = cumsum(closing_balance_sum))
LGD_Summary$recovery_perc <- if_else(LGD_Summary$payment_sum == 0, 0, (LGD_Summary$payment_sum/LGD_Summary$exposure_at_default_sum*-1))
LGD_Summary$cum_recovery_perc <- if_else(LGD_Summary$cumsum_payment == 0, 0, (LGD_Summary$cumsum_payment/LGD_Summary$exposure_at_default_sum*-1))

#Set up vintage view
#transpose data: used to plot actual vintages
triangle_payment_perc<-dcast(LGD_Summary, default_month ~ months_in_default, value.var = "recovery_perc")
triangle_payment<-dcast(LGD_Summary, default_month ~ months_in_default, value.var = "payment_sum")
triangle_payment_cum <- dcast(LGD_Summary, default_month ~ months_in_default, value.var = "cumsum_payment")
triangle_payment_cum_perc <- dcast(LGD_Summary, default_month ~ months_in_default, value.var = "cum_recovery_perc")

###################Run-off triangle####################

#remove first col when completing triangle (based on monthly payment values)
triangle_payment2 <- triangle_payment[,-1]

#create a set with default dates and exposure at default per vintage

# require(devtools)

ead<-as.data.frame(LGD_Summary[(LGD_Summary$months_in_default==1),c(1,5)])

#double check
# library(xlsx)
# write.xlsx(x = triangle_payment_cum_perc, file = "triangle_payment_cum_perc.xlsx",
#       sheetName = "TestSheet", row.names = FALSE)

n=nrow(triangle_payment2) 

#calculate the vector of ratios used when completing triangle
f <- sapply(1:(n-1),
            function(i){
              sum(triangle_payment2[c(1:(n-i)),i+1])/sum(triangle_payment2[c(1:(n-i)),i])
            }
)

#complete triangle
for(k in 1:n){
  triangle_payment2[(n-k+1):n, k+1] <- triangle_payment2[(n-k+1):n,k]*f[k]
}

triangle_payment2 <- cbind(ead,triangle_payment2[,1:60])


###################LGD calculation#####################
#calculate the sum of each of these columns in dataframe (Note: adjust so that one can filter on number of vintages to use/sum)

final_recoveries <- data.frame(t(colSums(triangle_payment2[sapply(triangle_payment2, is.numeric)], na.rm = TRUE)))

divide <- function(x){
  (x / final_recoveries$exposure_at_default_sum)*(-1)
}

final_recoveries <- t(apply(final_recoveries[,-1], 1, divide))
final_recoveries <- rbind(final_recoveries,t(apply(final_recoveries, 1, cumsum))) #cumulative ratios


###################Discounting LGDs#####################

NPV_calculation_K <- npv_cf_calculation(cf_length =  prediction_period, cf = final_recoveries, 
                                        discount_rate = discount_rate_k)

LGD2_K <- data_frame(NPV_calculation_K) %>%
  rownames_to_column(., var = "months_in_default") %>%
  mutate(months_in_default = as.integer(months_in_default)) %>%
  mutate(lgd2 = 1 - cumsum(rev(NPV_calculation_K))) %>%
  mutate(lgd2 = lead(rev(lgd2)))%>%
  select(months_in_default, lgd2)
LGD2_K$lgd2[60] <- 1

##########Single LGD value (disc & undisc)##############

Final_receipting_value <- sum(final_recoveries[1,])
Final_LGD_value <- (1-sum(final_recoveries[1,]))
Final_discounted_receipting_value <- sum(NPV_calculation_K)
Final_discounted_LGD_value_K <- (1 - sum(NPV_calculation_K))


###############Graphical representation################

#set final curves to datasets used for graphs

default_month <- as.Date(c('2020-01-01','2020-01-01'))
temp <- data.frame(default_month)

final_recoveries = cbind.data.frame(default_month,final_recoveries) 

#cumulative view

#create subset
plot_table_k <- filter(triangle_payment_cum_perc, default_month >= as.Date('2014-01-01')) #date as input?
names(final_recoveries)[2:61] <- paste(1:60, sep="")
plot_table_k <- rbind(plot_table_k,final_recoveries[2,])

# load current cummulative recovery object and add to dataframe
load(file = "current_cum_recovery_k.rda")
plot_table_k <- rbind(plot_table_k, current_cum_recovery_k) # add current cummulartive recovery used for Kenya


#plot the cumulative recoveries & calculated curve
cohort.chart1 <- melt(plot_table_k, id.vars = "default_month")
colnames(cohort.chart1) <- c('vintage', 'month', 'recoveries')

p<- ggplot(cohort.chart1, aes(x=month, y=recoveries, group = vintage))
p + geom_line(size=0.5, alpha=1/2) +
  geom_point(size=0.5, alpha=1) +
  labs(title="Cumulative recoveries")

#monthly receipts

plot_table2_k <- filter(triangle_payment_perc, default_month >= as.Date('2014-01-01'))
plot_table2_k <- rbind(plot_table2_k,final_recoveries[1,])

# load current monthly recovery object and add to dataframe
load(file = "current_mon_recovery_k.rda")
plot_table2_k <- rbind(plot_table2_k, current_mon_recovery_k) # add current monthly recovery used for Kenya

cohort.chart1 <- melt(plot_table2_k, id.vars = "default_month")
colnames(cohort.chart1) <- c('vintage', 'month', 'recoveries')

p<- ggplot(cohort.chart1, aes(x=month, y=recoveries, group = vintage))
p + geom_line(size=0.5, alpha=1/2) +
  geom_point(size=0.5, alpha=1) +
  labs(title="Monthly recoveries")

# should form part of execution code
# create matrix object for lgd npv calc. 
# -----------------------------
rr_matrix_k <- lgd_results$mon_recovery[[1]] %>%
  filter(default_month >= '2020-01-01') %>%
  .[ ,2:61] %>%
  as.matrix(.) 
# -----------------------------
# =====






# UGANDA 
# ==============================================================================================

####################inputs##############################

data <- df

prediction_period <- 60   #Number of periods receipts are expected: GVR = 60
discount_rate_u <- 0.3453      #Rate used to discount receipts
#monthly_effective <- 0.2652534071  #what will be received as input? 
n <- 121                  #Number of vitages used in calculation of run-off factors

########################################################


#################data analysis##########################
#Sort data
data <- arrange(data, contract_key, pointintime_month) 

#only keep entries from default onwards(LGD modelling), receipting: 60 months
data<-data[!(data$default_flag==0) & (data$months_in_default <= prediction_period),]
#2 625 346obs, now: 1 627 940

#Set negative closing balances = 0
#See positive receipts as reversals
data$closing_balance[data$closing_balance < 0] <- 0

data <- data %>% filter(country == "Uganda")

#recalculate exposure_at_default
setDT(data)[, exposure_at_default2 := closing_balance[months_in_default == 1], by = contract_key]

#Summarize data
LGD_Summary<-data %>%
  group_by(default_month, months_in_default) %>%
  summarize(payment_sum= sum(payment, na.rm=TRUE), 
            closing_balance_sum = sum(closing_balance, na.rm = TRUE), 
            exposure_at_default_sum = sum(exposure_at_default2, na.rm = TRUE))

#calculate cumulative values and percentages
LGD_Summary<- LGD_Summary %>% group_by(default_month) %>% mutate(cumsum_payment = cumsum(payment_sum), cumsum_balance = cumsum(closing_balance_sum))
LGD_Summary$recovery_perc <- if_else(LGD_Summary$payment_sum == 0, 0, (LGD_Summary$payment_sum/LGD_Summary$exposure_at_default_sum*-1))
LGD_Summary$cum_recovery_perc <- if_else(LGD_Summary$cumsum_payment == 0, 0, (LGD_Summary$cumsum_payment/LGD_Summary$exposure_at_default_sum*-1))

#Set up vintage view
#transpose data: used to plot actual vintages
triangle_payment_perc<-dcast(LGD_Summary, default_month ~ months_in_default, value.var = "recovery_perc")
triangle_payment<-dcast(LGD_Summary, default_month ~ months_in_default, value.var = "payment_sum")
triangle_payment_cum <- dcast(LGD_Summary, default_month ~ months_in_default, value.var = "cumsum_payment")
triangle_payment_cum_perc <- dcast(LGD_Summary, default_month ~ months_in_default, value.var = "cum_recovery_perc")

###################Run-off triangle####################

#remove first col when completing triangle (based on monthly payment values)
triangle_payment2 <- triangle_payment[,-1]

#create a set with default dates and exposure at default per vintage
# require(devtools)
ead<-as.data.frame(LGD_Summary[(LGD_Summary$months_in_default==1),c(1,5)])

#double check
# library(xlsx)
# write.xlsx(x = triangle_payment_cum_perc, file = "triangle_payment_cum_perc.xlsx",
#            sheetName = "TestSheet", row.names = FALSE)

n=nrow(triangle_payment2) 

#calculate the vector of ratios used when completing triangle
f <- sapply(1:(n-1),
            function(i){
              sum(triangle_payment2[c(1:(n-i)),i+1])/sum(triangle_payment2[c(1:(n-i)),i])
            }
)

#complete triangle
for(k in 1:n){
  triangle_payment2[(n-k+1):n, k+1] <- triangle_payment2[(n-k+1):n,k]*f[k]
}

triangle_payment2 <- cbind(ead,triangle_payment2[,1:60])


###################LGD calculation#####################
#calculate the sum of each of these columns in dataframe (Note: adjust so that one can filter on number of vintages to use/sum)

final_recoveries <- data.frame(t(colSums(triangle_payment2[sapply(triangle_payment2, is.numeric)], na.rm = TRUE)))

divide <- function(x){
  (x / final_recoveries$exposure_at_default_sum)*(-1)
}

final_recoveries <- t(apply(final_recoveries[,-1], 1, divide))
final_recoveries <- rbind(final_recoveries,t(apply(final_recoveries, 1, cumsum))) #cumulative ratios


###################Discounting LGDs#####################

NPV_calculation_U <- npv_cf_calculation(cf_length =  prediction_period, cf = final_recoveries, 
                                        discount_rate = discount_rate_u)

LGD2_U <- data_frame(NPV_calculation_U) %>%
  rownames_to_column(., var = "months_in_default") %>%
  mutate(months_in_default = as.integer(months_in_default)) %>%
  mutate(lgd2 = 1 - cumsum(rev(NPV_calculation_U))) %>%
  mutate(lgd2 = lead(rev(lgd2))) %>%
  select(months_in_default, lgd2)
LGD2_U$lgd2[60] <- 1

##########Single LGD value (disc & undisc)##############

Final_receipting_value <- sum(final_recoveries[1,])
Final_LGD_value <- (1-sum(final_recoveries[1,]))
Final_discounted_receipting_value <- sum(NPV_calculation_U)
Final_discounted_LGD_value_U <- (1 - sum(NPV_calculation_U))

###############Graphical representation################

#set final curves to datasets used for graphs

default_month <- as.Date(c('2020-01-01','2020-01-01'))
temp <- data.frame(default_month)

final_recoveries = cbind.data.frame(default_month,final_recoveries) 

#cumulative view

#create subset
plot_table_u <- filter(triangle_payment_cum_perc, default_month >= as.Date('2014-01-01')) #date as input?
names(final_recoveries)[2:61] <- paste(1:60, sep="")
plot_table_u <- rbind(plot_table_u,final_recoveries[2,])

# load current cummulative recovery object and add to dataframe
load(file = "current_cum_recovery_u.rda")
plot_table_u <- rbind(plot_table_u, current_cum_recovery_u) # add current cummulartive recovery used for Uganda


#plot the cumulative recoveries & calculated curve
cohort.chart1 <- melt(plot_table_u, id.vars = "default_month")
colnames(cohort.chart1) <- c('vintage', 'month', 'recoveries')

p <- ggplot(cohort.chart1, aes(x=month, y=recoveries, group = vintage))
p + geom_line(size=0.5, alpha=1/2) +
  geom_point(size=0.5, alpha=1) +
  labs(title="Cumulative recoveries")

#monthly receipts

plot_table2_u <- filter(triangle_payment_perc, default_month >= as.Date('2014-01-01'))
plot_table2_u <- rbind(plot_table2_u,final_recoveries[1,])

# load current monthly recovery object and add to dataframe
load(file = "current_mon_recovery_u.rda")
plot_table2_u <- rbind(plot_table2_u, current_mon_recovery_u) # add current monthly recovery used for Uganda

cohort.chart1 <- melt(plot_table2_u, id.vars = "default_month")
colnames(cohort.chart1) <- c('vintage', 'month', 'recoveries')

p<- ggplot(cohort.chart1, aes(x=month, y=recoveries, group = vintage))
p + geom_line(size=0.5, alpha=1/2) +
  geom_point(size=0.5, alpha=1) +
  labs(title="Monthly recoveries")

# should form part of execution code
# create matrix object for lgd npv calc. 
# -----------------------------
rr_matrix_u <- lgd_results$mon_recovery[[2]] %>%
  filter(default_month >= '2020-01-01') %>%
  .[ ,2:61] %>%
  as.matrix(.) 
# -----------------------------
# ==============================================================================================


# combine results of countries in list for front-end implementation
#----------------------------------------------------------------------------------------------
lgd_results <- list("country" = c("Kenya","Uganda"), 
                    "discount_rate" = c(discount_rate_k, discount_rate_u), 
                    "discounted_lgd" = c(Final_discounted_LGD_value_K, Final_discounted_LGD_value_U),
                    "cum_recovery" = list(plot_table_k, plot_table_u), 
                    "mon_recovery" = list(plot_table2_k, plot_table2_u),
                    "lgd2" = list(LGD2_K, LGD2_U),
                    "rr_cf" = list(rr_matrix_k, rr_matrix_u))

# ,
# "rr_cf" = list(rr_k, rr_u)

write_rds(lgd_results, "lgd_results.rds")
#----------------------------------------------------------------------------------------------

}

