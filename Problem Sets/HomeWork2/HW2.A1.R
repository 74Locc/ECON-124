# Eliseo Vega
# ECON-124
# Professor Leung
# January 21, 2022

########        Homework 2A     ########
rm(list = ls()) # clearing data frame\
getwd() # getting working directory
##setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1")
setwd("/Users/eliseovega/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork2")


####### Question 1  #######
NBA <- read.csv("NBA.csv") #reading in data frame

# Converting all categorial variables into factors
NBA$Player <- factor(NBA$Player)
levels(NBA$Player)

NBA$NBA_Country <- factor(NBA$NBA_Country)
levels(NBA$NBA_Country)

NBA$Tm <- factor(NBA$Tm)
levels(NBA$Tm)

#######  Question 2  #######
table(is.na(NBA)) # There is 8 observations that have an NA 

# Omitting the NA observations from dataframe
NBA <- na.omit(NBA)
table(is.na(NBA))

#### 3 #######

hist(NBA$Salary, breaks = 15, main = "NBA Salary Distribution", ylab = "Frequency", xlab = "Salary", col = "skyblue3", border = "black")
abline(v = mean(NBA$Salary), col = "Red")


###### 4 ######

MoreThan50 <- NBA[NBA$G >= 50,]

MoreThan50$Player[which.max(MoreThan50$PER)]
MoreThan50$PER[which.max(MoreThan50$PER)]

MoreThan50$Player[which(MoreThan50$TS == max(MoreThan50$TS))]
MoreThan50$TS[which.max(MoreThan50$TS)]

MoreThan50$Player[which(MoreThan50$VORP == max(MoreThan50$VORP))]
MoreThan50$VORP[which.max(MoreThan50$VORP)]


###### 5 #########


length(which(NBA$PER > 28.2))




####### 6 #########
 


###### 7
ols <- glm(log(Salary)~ ., data = training_set[,-1])
