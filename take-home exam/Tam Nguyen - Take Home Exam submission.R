# SETUP-------

# this package helps to install and load packages at the same time.
install.packages("simpleSetup")
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "gmodels", "reprex", "datapasta")
library_install(packages)


# Question 2c -------------------------------------------------------------

BMI <- data.frame(
        Year = c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
                 2005),
         BMI = c(53, 54, 55, 56, 58, 60, 63, 64, 62, 62, 62)
)

barplot(BMI$BMI, BMI$Year, names.arg = BMI$Year, space = 1.5, ylab = "BMI", xlab = "Year", ylim = c(0,70)) # drawing a bar plot is the best display of the data.

# Question 3a -------------------------------------------------------------
#input data
reproduce <- data.frame(
  Reproductive.Success = c(0, 1, 2, 3, 4, 5, 6, 7),
  Female = c(30, 25, 3, 6, 8, 6, 0, 4),
  Male = c(38, 17, 7, 6, 4, 10, 2, 0)
)

# create a function to calculate mean

cal_mean <- function(x) sum(x)/length(x)
cal_mean(reproduce$Female)
cal_mean(reproduce$Male)
# male: 10.5, female: 10.25 ==> male have higher mean reproductive success

mean(reproduce$Male)

# Question 3c -------------------------------------------------------------
cal_var <- function(x) {
  mean_inner <- sum(x)/length(x)
  sum((x - mean_inner)^2)/length(x)
  
}
cal_var(reproduce$Female) # variance of female: 105.69
cal_var(reproduce$Male) # variance of male: 132 

# Question 3d -------------------------------------------------------------
cal_median <- function(x) x[length(x)/2]

cal_median(reproduce$Female) #median of female = 6
cal_median(reproduce$Male) # median of male = 6

# Question 7 --------------------------------------------------------------

# b.
# create the expected frequency table
taracua <- as.table(rbind(c(3, 37), c(31, 9)))
dimnames(taracua) <- 
  list(numLabaris = c("Unharmed", "Immobilized"),
       party = c("Blue Workers","White Workers"))
Xsq <- chisq.test(taracua)
# calculate the expected frequency
Xsq$expected

# c. I can do a X^2 contingency test and fisher test at the same time.


fisher.test(taracua)

CrossTable(taracua, chisq = TRUE)
# chisq test = 40.10
# d. reject the null because p is very significant. P < 0.05


# Question 8 --------------------------------------------------------------

# z = (x - μ) / σ:
# z = (0.65 - 0.569)/0.068 => 1.1912
# P ( Z>1.19 )=1−P ( Z<1.19 )=1−0.883=0.117 
# a. 11.7 of states will have more than 65% of their traffic fatalities from drunk driving.
# b. for 25 percentile, z score is -0.675. Therefore, the result is 0.52 or 52% of death is due to driving


# Question 9 --------------------------------------------------------------

# a. A standard normal distribution is a normal distribution with a mean of 0 and a standard deviation of 1.

# b.
# write a normal distribution function.

normal <- function(mu, s) {
  x <- seq(-4, 4, length=200) # x extends from -4 to 4
  y <- (1/(s * sqrt(2*pi))) * exp(-((x-mu)^2)/(2*s^2)) # y follows the formula of the normal distribution: f(Y)
  plot(x,y, type="l", lwd=2, xlim = c(-3.5,3.5)) # draw the graph
}
normal(0, 1)

# c. add vertical line

normalCriticalTest <- function(mu, s) {
  x <- seq(-4, 4, length=200) # x extends from -4 to 4
  y <- (1/(s * sqrt(2*pi))) * exp(-((x-mu)^2)/(2*s^2)) # y follows the formula of the normal distribution: f(Y)
  plot(x,y, type="l", lwd=2, xlim = c(-3.5,3.5))
  abline(v = c(-1.96, 1.96), col="red") # draw the graph, with 2.5% surface to either side of the mean
  
}
normalCriticalTest(0, 1)
