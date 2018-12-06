library(readr)
mammals <- read_csv("workshop/mammals.csv", 
                    na = "")
View(mammals)

library(skimr)
library(formattable)
table(mammals$continent) 

mammals$continent <- str_replace(mammals$continent, "Af", "AF")

#### 4.
# c. What is the test statistic for the test?
10/12

# d. What is the P-value for the test?
(2*(0.0002+0.003+0.016))
# the p value is 0.0384


# 3 -----------------------------------------------------------------------
library(readr)
fruitflies <- read_csv("workshop/fruitflies.csv")
View(fruitflies)
library(tidyverse)

library(readr)
bird <- read_csv("BirdWindowCrash.csv")
View(bird)

proportionDeathBird <- bird %>% 
  group_by(angleDuringBirdCrash) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(expected = sum(n)/n_distinct(angleDuringBirdCrash),
         expectedDeath = 1/3)

CrossTable(proportionDeathBird$expected, expected = TRUE, prop.t = TRUE)  
chisq.test(proportionDeathBird$angleDuringBirdCrash, proportionDeathBird$expected)
