
rm(list=ls())
require(data.table)

# read data from CSV file and only keep useful columns
downloadData <- function(csv_file) {
  data <- data.table(read.csv(csv_file, skip=1))
  col.names <- c('id','member_id','loan_amnt','term','list_d','int_rate','grade','sub_grade','annual_inc','purpose','addr_city','addr_state','dti','earliest_cr_line','fico_range_low','fico_range_high','policy_code','pub_rec','pub_rec_bankruptcies')
  data.modified <- data[, col.names, with=FALSE]
  return(data.modified)
}

# Wrangling data and cleaning it up
cleanUpData <- function(data) {
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
  
  # exluding accounts with income greater than $500,000
  data <- data[annual_inc<500000]
  
  return(data)
}

# outputs all the data prepared into one data.table for plotting and analysis
wrangleData <- function() {
  data.a <- downloadData('/Users/sinahab/Documents/Development/GroupLend/Data/Data_final/LoanStats3a_securev1.csv')
  data.a <- cleanUpData(data.a)
  cat("Downloaded and cleaned up dataset_a.\n")
  
  data.b <- downloadData('/Users/sinahab/Documents/Development/GroupLend/Data/Data_final/LoanStats3b_securev1.csv')
  data.b <- cleanUpData(data.b)
  cat("Downloaded and cleaned up dataset_b.\n")
  
  data.c <- downloadData('/Users/sinahab/Documents/Development/GroupLend/Data/Data_final/LoanStats3c_securev1.csv')
  data.c <- cleanUpData(data.c)
  cat("Downloaded and cleaned up dataset_c.\n")
  
  data <- rbind(data.a, data.b)
  data <- rbind(data, data.c)
  cat("Finalized dataset is ready.\n")
  
  return(data)
}

# handles the date ranges
fixDates <- function( data = wrangleData() ) {

  # the date intervals
  dates <- as.Date(c("2007-06-01", "2008-01-01", "2008-06-01", "2009-01-01", "2009-06-01", "2010-01-01", "2010-06-01", "2011-01-01", "2011-06-01", "2012-01-01", "2012-06-01", "2013-01-01", "2013-06-01", "2014-01-01", "2014-06-01"))
  
  # # finding the upper-bound of the date interval that this loan belongs to: min(greater)
  # which((data$list_date[10]-dates) == min((data$list_date[10]-dates)[data$list_date[10]-dates>0]))
  # # finding the lower-bound of the date interval that this loan belonds to: max(lesser)
  # which((data$list_date[10]-dates) == max((data$list_date[10]-dates)[data$list_date[10]-dates<0]))
  
  setkey(data, list_date)
  data[ , c("date_lower_bound_int", "date_upper_bound_int") := list(
    which((list_date-dates) == min((list_date-dates)[list_date-dates>0])),
    which((list_date-dates) == max((list_date-dates)[list_date-dates<0]))
    ), by=list_date
  ]
  
  setkey(data, date_lower_bound_int)
  data[, date_lower_bound:=as.character(dates[date_lower_bound_int]), by=date_lower_bound_int]
  
  setkey(data, date_upper_bound_int)
  data[, date_upper_bound:=as.character(dates[date_upper_bound_int]), by=date_upper_bound_int]
  
  return(data)
}

