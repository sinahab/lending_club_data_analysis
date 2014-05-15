
rm(list=ls())

require(data.table)
require(ggplot2)

# read data from CSV file and only keep useful columns
getData <- function(csv_file) {
  data <- data.table(read.csv(csv_file, skip=1))
  col.names <- c('id','member_id','loan_amnt','term','list_d','int_rate','grade','sub_grade','annual_inc','purpose','addr_city','addr_state','dti','earliest_cr_line','fico_range_low','fico_range_high','policy_code','pub_rec','pub_rec_bankruptcies')
  data.modified <- data[, col.names, with=FALSE]
  return(data.modified)
}

cleanUpData <- function(data) {
  # dealing with int_rate
  setkey(data, int_rate)
  data[, interest_rate:=as.numeric(substr(as.character(int_rate),1,6)), by='int_rate']
  data[, int_rate:=NULL]
  return(data)
}

# pull in data
data <- getData('/Users/sinahab/Documents/Development/GroupLend/Data/Data_final/LoanStats3a_securev1.csv')

# Wrangling data
# cleaning up interest rates
setkey(data, int_rate)
data[, interest_rate:=as.numeric(substr(as.character(int_rate),1,6)), by='int_rate']
data[, int_rate:=NULL]
data <- data[!is.na(interest_rate)]

# cleaning up id
setkey(data, id)
data[, primary_id:=as.character(id), by='id']
data[, id:=NULL]

# finding midpoint of fico range
setkey(data, fico_range_low, fico_range_high)
data[, fico:=mean(c(fico_range_low,fico_range_high)), by="fico_range_low,fico_range_high"]
data[, fico_range_low:=NULL]
data[, fico_range_high:=NULL]

# fixing the credit history length
setkey(data, earliest_cr_line)
data[, earliest_credit_line:=as.Date( substr(as.character(earliest_cr_line),1,10),"%Y-%m-%d")]
data <- data[,earliest_cr_line:=NULL]
data <- data[!is.na(earliest_credit_line)]
# todays date
today <- Sys.Date()
setkey(data, earliest_credit_line)
data[, credit_history_length_in_days:=today-earliest_credit_line, by=earliest_credit_line]
data[, earliest_credit_line:=NULL]

# fixing the loan list data
setkey(data, list_d)
data[, list_date:=as.Date(as.character(list_d),"%Y-%m-%d")]
data[, list_d:=NULL]

# excluding loans where term is 60 months
data <- data[term==" 36 months"]



# TODO
# histogram of loan grades
# histogram of loan subgrades


# # plotting the interest rate vs. the grade
# ggplot(data=data.modified[!is.na(grade) & !is.na(interest_rate)]) +
#   geom_point(aes(x=grade, y=interest_rate), alpha=0.1) +
#   xlab('grade') + ylab('interest rate') +
#   ggtitle('interest rate vs. grade\n')
# 
# # plotting annual income vs. grade
# ggplot(data=data.modified[!is.na(annual_inc) & !is.na(grade) & annual_inc<2*10^6]) +
#   geom_point(aes(x=grade, y=annual_inc), alpha=0.1) +
#   xlab('grade') + ylab('annual income') +
#   ggtitle('annual income vs. grade\n')
# 
# # histogram of loan purposes
# ggplot(data=data.modified) +
#   geom_histogram(aes(x=purpose)) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle('histogram of loans broken down by purpose\n')
# 
# # histogram of address states
# ggplot(data=data.modified) +
#   geom_histogram(aes(x=addr_state)) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ylab('state') +
#   ggtitle('histogram of loans broken down by state\n')
# 
# 
# 
