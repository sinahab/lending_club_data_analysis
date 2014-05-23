

# # plotting the interest rate vs. the grade
# ggplot(data=data[!is.na(sub_grade) & !is.na(interest_rate)]) +
#   geom_point(aes(x=sub_grade, y=interest_rate), alpha=0.1) +
#   xlab('grade') + ylab('interest rate') +
#   ggtitle('interest rate vs. grade\n')

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