#
# 
# -------------------------------------------------------
#  Problem set 1
#
#
# the '#" is used to indicate a line is a comment. Use this in abundance.
#
# R code for day 1 
#
# 5e (at the end of the intro code)

# 5e

# enter data
number.convictions <- c(0:14)
frequency <- c(265, 49, 21, 19, 10, 10, 2, 2, 4, 2, 1, 4, 3, 1, 2)
# check the sum
sum(frequency) # 396, correct
# for the histogram we got to repeat the value (umber.convictions) with its frequency
raw.data <- rep(number.convictions, frequency)
hist(raw.data, las = 1, col = "red")








