#### Homework 1 ####
getwd()
setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1")
install.packages("readstata13")
library(readstata13)


######     Question 1    ######
## a.)
install.packages("readstata13")
library(readstata13)

fakenews <- read.dta13("fakenews.dta")
survey <- read.dta13("survey.dta")


######     Question 2    ######
help("na.omit")

# x = number of times pro-Trump articles were shared on Facebook
fakenews <- na.omit(fakenews)
x <- sum(fakenews$fb_share[fakenews$pro == "Trump"])
x

# y = number of times pro-Clinton articles were shared on Facebook
y <- sum(fakenews$fb_share[fakenews$pro == "Clinton"])
y

# Using cat() function to make a dynamic print message
# Multipying x and y by 1000 since number of articles was in 1000s
cat(x*1000,"pro-Trump articles were shared on Facebook, and ", y*1000, "pro-Clinton articles were shared, in this dataset.")
