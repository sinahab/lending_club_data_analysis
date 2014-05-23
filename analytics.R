
require(ggplot2)

# the date intervals
dates <- as.Date(c("2007-06-01", "2008-01-01", "2008-06-01", "2009-01-01", "2009-06-01", "2010-01-01", "2010-06-01", "2011-01-01", "2011-06-01", "2012-01-01", "2012-06-01", "2013-01-01", "2013-06-01", "2014-01-01", "2014-06-01"))

#------------------------------------------------------------------

# regression analysis between 1) interest rate, and 2) FICO score.
fit.fico <- lm(data$interest_rate ~ data$fico)
summary(fit.fico)

ggplot(data=data) +
  geom_point(aes(x=fico, y=interest_rate), alpha=0.05) +
  xlab('FICO score') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) FICO score\n')

ggplot(data=data) +
  geom_point(aes(x=fico, y=interest_rate), alpha=0.05) +
  facet_wrap(~ date_lower_bound, ncol=2) +
  xlab('FICO score') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) FICO score\nas broken down by loan list date\n')

#------------------------------------------------------------------

# regression analysis between 1) interest rate, and 2) debt-to-income ratio.
fit.dti <- lm(data$interest_rate ~ data$dti)
summary(fit.dti)

ggplot(data=data) +
  geom_point(aes(x=dti, y=interest_rate), alpha=0.05) +
  xlab('Debt-to-income ratio') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) dti ratio\n')
  
ggplot(data=data) +
  geom_point(aes(x=dti, y=interest_rate), alpha=0.05) +
  facet_wrap(~ date_lower_bound, ncol=2) +
  xlab('Debt-to-income ratio') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) dti ratio\nas broken down by loan list date\n')


#------------------------------------------------------------------

# regression analysis between 1) interest rate, and 2) loan amount.
fit.loan_amnt <- lm(data$interest_rate ~ data$loan_amnt)
summary(fit.loan_amnt)

ggplot(data=data) +
  geom_point(aes(x=loan_amnt, y=interest_rate), alpha=0.05) +
  xlab('loan amount ($)') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) loan amount\n')

ggplot(data=data) +
  geom_point(aes(x=loan_amnt, y=interest_rate), alpha=0.05) +
  facet_wrap(~ date_lower_bound, ncol=2) +
  xlab('loan amount ($)') + ylab('interest rate') +
  ggtitle('Plot of 1) interest rate vs. 2) loan amount\nas broken down by loan list date\n')

#------------------------------------------------------------------

# histogram of loan grades
ggplot(data=data) +
  geom_histogram(aes(x=grade)) +
  ggtitle('Histogram of loan grades\n')

# histogram of loan sub_grades
ggplot(data=data) +
  geom_histogram(aes(x=sub_grade)) +
  ggtitle('Histogram of loan sub-grades\n')

# plot of 1) interest rate vs. 2) subgrade
ggplot(data=data) +
  geom_point(aes(x=sub_grade, y=interest_rate), alpha=0.05) +
  xlab("loan sub-grade") + ylab("interest rate") +
  ggtitle("Plot of 1) interest rate vs. 2) loan sub_grade\n")

# plot of 1) interest rate vs. 2) annual income
ggplot(data=data) +
  geom_point(aes(x=annual_inc, y=interest_rate), alpha=0.05) +
  xlab("annual income") + ylab("interest rate") +
  ggtitle("Plot of 1) interest rate vs. 2) annual income\n")

#------------------------------------------------------------------

# Plotting
fit <- lm(data$interest_rate ~ data$dti + data$fico + data$loan_amnt + data$credit_history_length_in_days)
fitted(fit)

layout(matrix(1:4,2,2))
plot(fit)

ggplot(cbind(data$interest_rate, fitted(fit))) + geom_point(aes(x=interest_rate, y=fit))





