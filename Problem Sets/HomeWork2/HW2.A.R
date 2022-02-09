# Eliseo Vega
# ECON-124
# Professor Leung
# January 21, 2022

########        Homework 2A     ########
rm(list = ls()) # clearing data frame\
getwd() # getting working directory
##setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork2")
setwd("/Users/eliseovega/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork2")


#'*############### Question 1  ################*
NBA <- read.csv("NBA.csv") #reading in data frame

# Converting all categorical variables into factors
NBA$Player <- factor(NBA$Player)
levels(NBA$Player)

NBA$NBA_Country <- factor(NBA$NBA_Country)
levels(NBA$NBA_Country)

NBA$Tm <- factor(NBA$Tm)
levels(NBA$Tm)

typeof(NBA$Player)


#'*################  Question 2  #################*
is.na(NBA)
table(is.na(NBA)) # There is 8 observations that have an NA 

# Omitting the NA observations from data frame
NBA <- na.omit(NBA)
table(is.na(NBA)) # No observations with NA 


#'*#################  Question 3  #################*
## Using a histogram to Plot distribution of players salaries with 15 bins
## "breaks= 15" sets 15 bins in hist
## making red line to mark the mean of salaries
hist(NBA$Salary, main="Histogram of NBA Salaries",
     xlab= "Salaries", ylab= "Frequency", cex=0.5,
     col='skyblue3', border= "Black", breaks= 15)
abline(v=mean(NBA$Salary), col="red")
#'* What I learn from this plot is that most of the players make very low salaries*
#'* compared to the top earners. The mean is fairly low compared to the top earners*



## Same as above but getting log of Salary to see how that looks
hist(log(NBA$Salary), main="Histogram of NBA Salaries",
     xlab= "Salaries", ylab= "Frequency",
     col='purple', breaks= 15)
abline(v=mean(log(NBA$Salary)), col="red")



#'*##################  Question 4  ####################*
# PER: player efficiency rating (measure of a player's per-minute performance)
# TS: true shooting percentage (measure of how accurately they shot)
# VORP: value over replacement player - estimate of points per 100 team 
# possessions contributed above a replacement-level player

## Creating new data frame with only players who played with in atleast 50 games
MoreThan50 <- NBA[NBA$G >= 50,]

## Getting the player name with the max PER and the value
MoreThan50$Player[which.max(MoreThan50$PER)]
max(MoreThan50$PER)
#'*James Harden led the league in Player Eff. rating with 30.2 *

## Getting Player name with their TS that led league
MoreThan50$Player[which.max(MoreThan50$TS)]
max(MoreThan50$TS)
#'*Stephan Curry led the league in True shooting % with 67.5%*

## Getting Player name with their VORP that led the league
MoreThan50$Player[which.max(MoreThan50$VORP)]
max(MoreThan50$VORP)
#'*Lebron James led the league in VORP with 8.6 points*



#'*##################  Question 5  #####################*
## Scales packages to use percent function
install.packages("scales")
library(scales)
## Creating values that are Chef Currys PER, TS, VORP
ChefPER <- NBA$PER[NBA$Player == "Stephen Curry"]
ChefTS <- NBA$TS[NBA$Player == "Stephen Curry"]
ChefVORP <- NBA$VORP[NBA$Player == "Stephen Curry"]

## Getting the sum of each player who had higher respective stats then the Chef
sum(NBA$PER > ChefPER)
sum(NBA$TS > ChefTS)
sum(NBA$VORP > ChefVORP)

## Getting the Percentage of players who had better stats respectively then the Chef
percent(sum(NBA$PER > ChefPER)/nrow(NBA), accuracy = 0.1)
percent(sum(NBA$TS > ChefTS)/nrow(NBA), accuracy = 0.1)
percent(sum(NBA$VORP > ChefVORP)/nrow(NBA), accuracy = 0.1)
#'*The Percent of players who had a higher PER than Chef Curry is 1.4%*
#'*The Percent of players who had a higher TS than Chef Curry is 2.1%*
#'*The Percent of players who had a higher VORP than Chef Curry is 2.1%*





#'*#########################  Question 6  #############################*
## importing catTools to split the data set
install.packages('caTools')
library(caTools)
## Restricting data to American NBA players
USA <- NBA[NBA$NBA_Country == "USA",]

# Getting ratio of the intended split
# Getting a random split of true and false. True = Training set, False=test_set
Ratio <- round(0.9 *nrow(USA))
split <- sample.split(USA$Player, SplitRatio = Ratio)
sum(!split)
sum(split)

# Randomly splitting data into two data frames. 
# Two ways of doing this
# 'test_set' contains 10% or 37 of observations rounded to nearest int
test_set <- subset(USA, split == FALSE)
USA[!split,] # another way of doing this

# 'training_set' contains 90% or 335 of the observations
training_set <- subset(USA, split == TRUE)
USA[split,]


#'*#########################  Question 7  #############################*
### a.)
# Running an OLS regression of log salary on all variables except name
training_set<- within(training_set, rm(NBA_Country))
OLS <- glm(log(Salary)~ . -Player, data= training_set)
summary(OLS)

#'*The 5 Largest slope coefficients in magnitude are: *
#'*WS.48= 24.781*
#'*OBPM= -1.679*
#'*BPM= 1.370 *
#'*TS.= 4.722*
#'*TmUTA= -1.532*


## b.)
#'*TRB=1.335, meaning that for a unit increase in TRB its predicted player salary increases by 1.33% points *
#'*AST= 0.032 meaning that for a unit increase in AST its predicted player salary increases by 0.032% points*
#'*STL= -0.11, meaning that for a unit increase in STL its predicted player salary decreases by -0.11% points*
#'*BLK = -0.076 meaning that for a unit increase in BLK its predicted player salary decreases by -0.07% points*
#'*BPM= 1.37 meaning that for a unit increase in BPM its predicted player salary increases by 1.37% points*

## c.)
## Computing IS R^2 for training Set
test_set<- within(test_set, rm(NBA_Country))
OLS2 <- glm(log(Salary)~ . -Player, data= test_set)

1-OLS$deviance / OLS$null.deviance
#'*IS R^2 = 0.64, meaning that 64% of variance in test_set data set is by the covariates used*



## Computing OOS R^2 for test_set
logit_dev <- function(y, pred) {
  return(sum(y-pred)^2)
}
new_y <- test_set$

logit_pred <- predict(OLS, newdata=test_set, type="response") # logit predictions
mean_pred <- mean(training_set$FAIL) 

1 - logit_dev(new_y, logit_pred) / logit_dev(new_y, mean_pred)




#'*############## Question 8 ################*
### a.)
# Setting refe
naref(test_set)
naref(training_set)

### b.)

X <- model.matrix()















