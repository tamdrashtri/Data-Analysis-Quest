#
#
# Workshop 1 Introduction 
#
#

# get working directory (where all your files are stored and written to)
getwd()

# see which files there are in your working dir
list.files()

# ---- Oversized calculator ----------------------------------------------------
# follow the instructions on the webpage...
# --- Vectors ------------------------------------------------------------------

# 1 
t.vector <- c(1, 2, 5.5, 3, 18, -1, -1.9, -11, 0, 2)
# check if it is a vector
is.vector(t.vector)

# 2 
# 6th element
t.vector[5]
# or multiple elements.
t.select<- c(1, 5, 7)
t.vector[t.select]

# 3 use funciton length()
length(t.vector)
# get last element of vector
t.vector[length(t.vector)]

# 4 logical operator
t.vector> 4
# returns TRUE / FALSE

# where are they located?
which(t.vector > 4)  #now you get the locations in the vector

# 5 change element 5
t.vector[5] <- 99

# 6 missing value is NA, make 2nd position missing value
t.vector[2] <- NA
# check this
is.na(t.vector)

# 7 calculations
# make second variable.
t.v2 <- 5
# multiply the two 
t.vector * t.v2
t.v2 <- t.vector >= t.v2
#

# 8 combine into dataframe
 mydata <- data.frame(var1 = t.vector, var2 = rep(t.v2, length(t.vector)), 
                      stringsAsFactors = FALSE)
str(mydata)
names(mydata)

# 9 print single variable names



# --- Analyze vector of data: flying snakes ------------------------------------
# 1
snakes <- c(0.9, 1.4, 1.2, 1.2, 1.3, 2.0, 1.4, 1.6)
length(snakes)

# 2
# right closed
hist(snakes)
# left closed
hist(snakes, right = FALSE)

# 3 multiple Hrzt by 2pi to turn it into radiants
(snakes.rad <- 2 * pi * snakes)

# 4 get the mean
sum(snakes.rad) / length(snakes.rad)

# 5 check with function
mean(snakes.rad)
# same as 4

# 6 get variance, sample size and get the sd
(t.var <- sum( (snakes.rad - mean(snakes.rad))^2) / (length(snakes.rad) - 1))
(var(snakes.rad)) # check the variance

# sd is srt of variance
(t.sd <- sqrt(t.var))
(sd(snakes.rad)) # check


# 7 snakes sorted
snakes.rad.sort <- sort(snakes.rad)

# 8 get median
median(snakes.rad.sort)

# 9 get standard error
(t.se <- t.sd / sqrt(length(snakes.rad)))


# --- Missing data -------------------------------------------------------------
# 1 add an NA to snakes
snakes.na <-c(snakes, NA)

# 2 check length
length(snakes.na)

# 3 mean using length()
sum(snakes.na,na.rm = TRUE) / length(snakes.na)
# gives 1.222222

# 4 will go wrong as the length() is not the length minus NA's

# 5 remove the na's from the vector is one way..... 
snakes.no.na <- na.omit(snakes.na)


# ------ Anolis lizards -------------------------------------------------------
# 1 - 4

# check what files are in the working dir
getwd() # 
list.files()
# download the file anolis.csv
mydata <- read.csv("anolis.csv")	
str(mydata) # check the structure of the object x1

# 4 check is Island variable is a factor
is.factor(mydata$Island)
class(mydata$Island) # another way to do it, R often has several ways to solve
# problem
class(mydata)

# 5 check the first section of the data frame
head(mydata)
tail(mydata)

# 6 - 7
table(mydata$Ecomorph)
levels(mydata$Ecomorph)
# [1] ""              "Crown-Giant"   "Crown-Giant "  "Grass-Bush"    "Trunk"        
#  [6] "Trunk-Crown"   "Trunk-Crown "  "Trunk-Ground"  "Trunk-Ground " "Twig"   

# find the row with the typo
which(mydata$Ecomorph == "Crown-Giant ")

# 8
# check in the data frame and assign the right name
mydata$Ecomorph[which(mydata$Ecomorph == "Crown-Giant ")] <- "Crown-Giant"
levels(mydata$Ecomorph)

# Number of species in each Ecomorph category
table(mydata$Ecomorph)
# ""        Crown-Giant  Crown-Giant     Grass-Bush         Trunk   Trunk-Crown 
# 47            12             0            25             7            20 
# Trunk-Crown   Trunk-Ground Trunk-Ground           Twig 
# 1            30             1            11
# 9
levels(mydata$Ecomorph)
droplevels(mydata$Ecomorph)          
levels(mydata$Ecomorph)     
# hmm doesn't seem to work...

# 10
x <- read.csv("anolis.csv",stringsAsFactors=FALSE, strip.white = TRUE, na.strings = c("NA","") )

levels(x$Ecomorph)
# Check how many unqiue character strings there are
unique(x$Ecomorph)
# looks good!

# 12
table(x$Ecomorph)
# Crown-Giant   Grass-Bush        Trunk  Trunk-Crown Trunk-Ground         Twig 
#           12           25            7           21           31           11 
# most common is Trunk-Ground and most rare Trunk

# 13
table(x$Ecomorph, useNA = 'ifany')
# Crown-Giant   Grass-Bush        Trunk  Trunk-Crown Trunk-Ground         Twig 
#          12           25            7           21           31           11 
#        <NA> 
#          47 

# let's give them the name "none"
x$Ecomorph[ is.na(x$Ecomorph)] <- c("none")



# 14
# The variable "Island" records all islands on which a species occurs
# so "Jamaica" refers to those species ocurring exclusively on Jamaica
(Jamaica <- x$Species[x$Island=="Jamaica"])
length(unique(Jamaica))# 6
#                Species  Island     Ecomorph Series.Clade
# 112     Anolis garmani Jamaica  Crown-Giant      grahami
# 113     Anolis grahami Jamaica  Trunk-Crown      grahami
# 114    Anolis opalinus Jamaica  Trunk-Crown      grahami
# 115  Anolis lineatopus Jamaica Trunk-Ground      grahami
# 116 Anolis valencienni Jamaica         Twig      grahami
# 117  Anolis reconditus Jamaica         <NA>      grahami
length(x$Species[x$Island=="Jamaica"])




# 15
# All species occurring on Cuba
# (see Functions for character data on R help pages)
length(x$Species[grep("Cuba",x$Island)])

# [1] 63
# or even shorter
length(grep("Cuba",x$Island))
# [1] 63

# 16
# Tally ecomorphs on each island
table(x$Island,x$Ecomorph)

#                            Crown-Giant Grass-Bush Trunk Trunk-Crown Trunk-Ground Twig
# Bahamas                              0          0     0           3            0    0
# Carrot Rock                          0          0     0           0            1    0
# Cuba                                 6         15     1           7           13    4
# Cuba.Bahamas                         0          0     0           0            0    1
# Cuba.Bahamas.Other islands           0          0     0           0            1    0
# Desecheo                             0          0     0           0            1    0
# Grand Cayman                         0          0     0           1            0    0
# Hispaniola                           3          7     5           4            9    4
# Hispaniola.Bahamas                   0          0     1           0            0    0
# Inagua                               0          0     0           0            1    0
# Jamaica                              1          0     0           2            1    1
# Little Cayman                        0          0     0           1            0    0
# Mona                                 0          0     0           0            1    0
# Navassa                              0          0     0           1            0    0
# Northern Lesser Antilles             0          0     0           0            0    0
# Providencia                          0          0     0           0            0    0
# Puerto Rico                          1          3     0           2            3    1
# Puerto Rico Bank                     1          0     0           0            0    0
# San Andres                           0          0     0           0            0    0
# Southern Lesser Antilles             0          0     0           0            0    0
# St. Croix                            0          0     0           0            0    0

# Tally ecomorphs on just the four big islands
table(x$Ecomorph[grep("Cuba",x$Island)])
# Crown-Giant   Grass-Bush        Trunk  Trunk-Crown Trunk-Ground         Twig 
#           6           15            1            7           14            5 
table(x$Ecomorph[grep("Jamaica",x$Island)])
# Crown-Giant  Trunk-Crown Trunk-Ground         Twig 
#           1            2            1            1 
table(x$Ecomorph[grep("Hispaniola",x$Island)])
# Crown-Giant   Grass-Bush        Trunk  Trunk-Crown Trunk-Ground         Twig 
#           3            7            6            4            9            4 
table(x$Ecomorph[grep("Puerto Rico",x$Island)])
# Crown-Giant   Grass-Bush  Trunk-Crown Trunk-Ground         Twig 
#           2            3            2            3            1 


# All together now to get locations of the different ecomorphs
location.big.island <- unique(c(grep("Cuba",x$Island),grep("Jamaica",x$Island),
              grep("Hispaniola",x$Island),grep("Puerto Rico",x$Island)))

# unique() is just an extra, not needed here, byt provides an extra check you 
# don't double count
ii <- c(grep("Cuba",x$Island),grep("Jamaica",x$Island),
        grep("Hispaniola",x$Island),grep("Puerto Rico",x$Island))


table(x$Island[location.big.island],x$Ecomorph[location.big.island])
#                            Crown-Giant Grass-Bush Trunk Trunk-Crown Trunk-Ground Twig
# Cuba                                 6         15     1           7           13    4
# Cuba.Bahamas                         0          0     0           0            0    1
# Cuba.Bahamas.Other islands           0          0     0           0            1    0
# Hispaniola                           3          7     5           4            9    4
# Hispaniola.Bahamas                   0          0     1           0            0    0
# Jamaica                              1          0     0           2            1    1
# Puerto Rico                          1          3     0           2            3    1
# Puerto Rico Bank                     1          0     0           0            0    0

# None of the species are found on more than one large island, so this should work
x$Bigisland<-x$Island
x$Bigisland[grep("Cuba",x$Island)] <- "Cuba"
x$Bigisland[grep("Jamaica",x$Island)] <- "Jamaica"
x$Bigisland[grep("Hispaniola",x$Island)] <- "Hispaniola"
x$Bigisland[grep("Puerto Rico",x$Island)] <- "Puerto Rico"

y <- x[x$Bigisland=="Cuba" | x$Bigisland=="Jamaica" | 
         x$Bigisland=="Hispaniola" | x$Bigisland=="Puerto Rico", ]

table(y$Bigisland,y$Ecomorph)
#             Crown-Giant Grass-Bush Trunk Trunk-Crown Trunk-Ground Twig
# Cuba                  6         15     1           7           14    5
# Hispaniola            3          7     6           4            9    4
# Jamaica               1          0     0           2            1    1
# Puerto Rico           2          3     0           2            3    1

# not that this is not very elegantly coded but it works,

# 17 Most frequent ecomorph among the species not occurring on large islands
# lets use 'location.big.island' to exclude all of these from the data set
# by selecting all rows which are not location.big.island
small.isl <- x[-location.big.island, ]
nrow(x)

table(small.isl$Ecomorph)
# Crown-Giant        Trunk  Trunk-Crown Trunk-Ground         Twig 
#  1                  1            6            5              1 

# Trunk-Crown most common









# ---- the end