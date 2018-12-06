library(readr)
library(tidyverse)
library(formattable)
mammals <- read_csv("mammals.csv", 
                    na = "")
glimpse(mammals)
# ============================================================================
# ---- data set 1: mammals -------
# Mammals

mammalContinent <- table(mammals$continent) 

str_replace(mammals$continent, "Af", "AF")

table(mammals$status)

table(mammals$continent,mammals$status)

## visualisation ------
barplot(table(mammals$continent),col="blue", ylab = "Frequency", 
        cex.names=0.8, ylim = c(0,1600))

mammalSorted <- sort(mammalContinent, decreasing = TRUE)

barplot(mammalContinent)

barplot(mammalSorted)

# 3.
hist(mammals$mass.grams)

# 4.
mammals$logmass <- log(mammals$mass.grams, base = 10)
hist(mammals$logmass)

# 5.
hist(mammals$logmass, breaks = seq(0, 10, 2))

hist(mammals$logmass, freq = FALSE, probability = TRUE)

# Visualize associations between variables ------

mammalStatus <- table(split(mammals$logmass,mammals$status))

boxplot(logmass ~ status, data = mammals, boxwex = 0.5)

boxplot(logmass ~ status, data = mammals, wi)

boxplot( split(mammals$logmass,mammals$status), boxwex=0.5, ylab = "log mass",
         las = 1)

# 5. multiple histogram
par(mfrow=c(3,1)) 
hist(status ~ logmass , data = mammals)
hist(mpg)
hist(disp)

ggplot(mammals, aes(x = logmass)) +
  geom_histogram(color = "grey30", fill = "white") 

mammals %>% 
  group_by(status) %>% 
  summarise(median_size = median(logmass, na.rm = TRUE)

ggally_facethist(mammals, mapping = ggplot2::aes(x = logmass, y = status), binwidth = 0.05)

