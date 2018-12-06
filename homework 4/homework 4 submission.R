# NEED THIS FOLLOWING PROCEDURE FOR SETTING UP NECESSARY PACKAGES ----
install.packages("simpleSetup")
library(simpleSetup)

# Load packages, install if needed
packages <- c('tidyverse', 'skimr', "GGally", "readr", "gmodels", "infer", "psych")
library_install(packages)


# Q1 ----------------------------------------------------------------------
dog <- read_csv("homework 4/PrairieDogMultipleMating.csv")

# a. this function can make a contigency table and get the result
CrossTable(dog$matingFrequency, dog$gaveBirth, expected = TRUE)
# you can see the expected frequencies for each cell in in the second row.

# b. no it does not meet the assumptions of the chi squared test. the expected values are below 1 for two cells and below 5 for more than 20% of the cells. This violate the assumption "No more than 20% of the expected counts are less than 5 and all individual expected counts are 1 or greater" (Yates, Moore & McCabe, 1999, p. 734)".A possible way to meet the assumption is to make binary options, like "1 or less" and "1 or more" number of times female mate.

# c. no. there is no evidence that having more mates can affect the probability of giving birth.

# d.
# calculate chi squared contingency test
obs_chisq <- dog %>%
  specify(gaveBirth ~ matingFrequency) %>% # alt: response = origin, explanatory = season
  calculate(stat = "Chisq") 

obs_chisq

# e.
# based on that reasoning, create the matrix:
dog1 <- matrix(c(81, 168, 6, 8),
       nrow = 2,
       dimnames = list(Guess = c("1", "more than 1"),
                       Truth = c("gave birth", "doesn't gave birth")))

fisher.test(dog1, alternative = "greater")
# p value is 0.86, not significant enough. the p-value is higher than the threshold value, so we cannot conclude that mating with more males increases the probability of giving birth. 

# f. make a table of expected frequency using R
CrossTable(dog1, expected = TRUE) # a special formula for it.



# Q2 ----------------------------------------------------------------------
# a. it's an observational study.

daycare <- read_csv("ALLDaycare.csv")

# b.
# proportion of ALL children with significant social activity: 1020/1272 => 0.8019
# proportion of without ALL children with significant social activity: # 5343/6238 => 0.8565

# c. What is the odds ratio for ALL, comparing the groups with and without significant social activity?;

# odd ratio = odds of success in one group/odds of success in the second group

# ALL kids: 
# 1020/1272 => 0.8019 ;with significant activity
# 1 - 0.802 => 0.198 ;without significant activity
# W/o ALL: 
# 5343/6238 => 0.8565 ;;with significant activity
# 1 - 0.857 => 0.143 ;;without significant activity

# odd ratio = odds of success in one group/odds of success in the second group
# odds ratio = (0.8019*0.143)/(0.198*0.8565) => 0.6762

# d. What is the 95% confidence interval for this odds ratio?
# using the formula provided by the textbook, we have the following CI: 0.5807 to 0.7917

# e. Yes. this CI indicates there is an association.
# f. Some confounding variables can be children's gender, their geography, or some activities like smoking.

# Q3 ----------------------------------------------------------------------


library(readr)
widow <- read_csv("homework 4/WidowHealth.csv")
View(widow)

# a. contingency test
# a more efficient way to do this test is using CrossTable
CrossTable(widow$health_deterioration, widow$widowed, expected = TRUE, prop.t = TRUE)

# we could still use the traditional way
with(widow, table(widow$health_deterioration, widow$widowed, useNA = "ifany"))

# chi^2 is 11.18
# p value is 0.003. this is significant so we reject the null hypothesis and conclude that there is a correlation between health deterioration and being widowed.

# b. 
widowTransformed <- t(with(widow, table(widow$health_deterioration, widow$widowed, useNA = "ifany")))

chisq.test(widowTransformed)

# it does not matter if you switch rows to columns. The result is the same.

# Q4 ----------------------------------------------------------------------

# a. 1-0.53 = 0.47
# b. Using the binomial formula we get p = 0.53, n = 5, success rate = 3 --> P(X > x) = 0.55
# c. standard deviation =sqrt(n*p*(1-p)) = 1.116
# d. mean = n*p =263*0.53=139.39, standard deviation =sqrt(n*p*(1-p)) =sqrt(263*0.53*(1-0.53)) = 8.094029, P(X<150) = P((X-mean)/s <(150-139.39)/8.094029) = P(Z<1.31) --> P=0.9049
# Q5. ----------------------------------------------------------------------
# z = (x - μ) / σ:
# z = (0.65 - 0.569)/0.068 => 1.1912
# P ( Z>1.19 )=1−P ( Z<1.19 )=1−0.883=0.117 
# a. 11.7 of states will have more than 65% of their traffic fatalities from drunk driving.
# b. for 25 percentile, z score is -0.675. Therefore, the result is 0.52 or 52% of death is due to driving


# Q6 ----------------------------------------------------------------------

# a. Inferring from the confidence interval formula, they have different sample sizes. They might also go in different times and observe outliers that affect the mean and SD value. 

# b. Research 1 might have the larger sample since the confidence interval is smaller.

# c. yes, pretty certain.

# Q7. ----------------------------------------------------------------------

walk <- read_csv("homework 4/WalkingInCircles.csv")

# a. 
hist(walk$angle, breaks=20)
# they tend to turn to one angle more on average.

# c. Based on your results in (b) is the following statement justified: “People do not have a tendency to turn more in one direction, on average, than the other direction”? Explain.

# this statement makes sense since the p value is not significant, so we do not reject the null hypothesis.

# d.
hist(walk$angle)
hist(walk$angle, breaks=20)
# the new graph changes my interpretation because i can see more clearly what's more present and the distribution.

# e.
# summary of data
describe(walk$angle) # summary of the data ; need psych package

# find mode using mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(walk$angle)

# mean: -0.14, mode = -0.15, median = -0.07, standard deviation = 1.69 and standard error = 0.44

