# Eliseo Vega
# ECON-124
# Professor Leung
# January 15, 2022

####      Homework 2      ####
getwd()
##setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1&2")
setwd("/Users/eliseovega/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1&2")
rm(list = ls())

#installing special package to read in .dta file
install.packages("readstata13")
library(readstata13)

fakenews <- read.dta13("fakenews.dta")
survey <- read.dta13("survey.dta")

######## Question 6
#Testing what happens when I assign function Categorical to a Variable
survey$Race <- factor(survey$Race)
levels(survey$Race)
boxplot(UseSocialMedia ~ Race, data=survey)

##1 Hot encoding
survey$Black <- survey$Race == 3

### a.)
#Running a logistic regress model to see what variables are associated with whether 
#respondent uses social media. Variables include if Voted for Trump, if they are a Male
# years of education and Age
Logit_Model <- glm(UseSocialMedia ~ Voted_Trump + Educ_Years + Male + Age, data = survey, family= "binomial")
summary(Logit_Model)

### b.)
# Finding fit of the regression
# Formula for R^2= 1-(Deviance/Null Deviance)
1 - ((Logit_Model$deviance) / (Logit_Model$null.deviance))
#= 0.1101 This means that only about 11% of variation in the data set is explained
#         by the covariates I used.
#         This also means their is 89% more unobserved factors driving variation on
#         if a person uses social media. This model does not fit the data well


### c.)
(exp(coef(Logit_Model)["Voted_Trump"])-1)*100
# According to my Model if the person Voted for Trump, its predicted their is a 9.1% increase
# in the log odds that the person uses social media



###### Question 7
# Dataframe 'Heard', a data frame of respondents who heard about the article
Heard_About <- survey[(survey$Heard == "Yes"),]

# creating the Barlabel of truth type as categorical variables
Heard_About$BarLabel <- factor(Heard_About$BarLabel)
levels(Heard_About$BarLabel)

#creating a barplot suming up each row of every category from Barlabel
truth_type <- tapply(rep(1, nrow(Heard_About)), Heard_About$BarLabel, sum)
barplot(sort(truth_type),las=2)
#=> About 600 artciles in Placebo, 750 in SmallTrue, 1,500 in Fake, and 3,000 in BigTrue.

#creating a barplot suming up each row of every category from ThoughtTrue
thought_true <- tapply(rep(1, nrow(Heard_About)),Heard_About$ThoughtTrue, sum)
barplot(sort(thought_true),las=2)
#=> about 3,750 of people who heard of articles believed they were true

# Bonus Question
#sets the plots in the same figure
brandcol <- c("blue", "red", "yellow", "green")
par(mfrow=c(1,2))
barplot(sort(truth_type),las=2, col=brandcol)
barplot(sort(thought_true),las=2, col=brandcol)




#######   Question 8
### a.)
# Defining a new dataframe of observations for truth type is either fake or placebo
fake_plac <- survey[survey$BarLabel == "Placebo" | survey$BarLabel == "Fake",]

fake_plac$is_Placebo <- fake_plac$BarLabel == "Placebo"
fake_plac$is_Fake <- fake_plac$BarLabel == "Fake"
fake_plac$HEARD <- fake_plac$Heard == "Yes"
fake_plac$Unsure <- fake_plac$Heard == "Not sure"
fake_plac$Not_heard <- fake_plac$Heard == "No"


model <- glm(is_Fake ~ HEARD + Unsure, data = fake_plac, family = 'binomial')
summary(model)



#######   Question 9
## a.)
# Did not know how to create a variable where I could give multiple conditions
# and set 3 different numeric values based on those conditions
# so I created 2 parts to make the Variable K
# First Part is I gave three columns for the 3 condtions all = TRUE
survey$correctTrue <- survey$ThoughtTrue== "Yes" & (survey$BarLabel== "Small True" | survey$BarLabel== "Big True")
survey$correctFalse <- survey$ThoughtTrue == "No" & survey$BarLabel == "Fake"
survey$correctNotsure <-  survey$ThoughtTrue == "Not sure"
# I then created a nested ifelse stament based on the individual columns
survey$K <- ifelse((survey$correctTrue | survey$correctFalse) == TRUE, 1, ifelse(survey$correctNotsure == TRUE, 0.5,0))

## b.)
# Using party variable 1,2 = Democrat
survey$Demo <- survey$Party == 1 | survey$Party == 2

# checking if results match up
table(survey$Democrat)
table(survey$Demo)

# Using party variable 6,7 = Republican
survey$Repo <- survey$Party == 6 | survey$Party == 7

# checking if results match up
table(survey$Republican)
table(survey$Repo)
## The baseline category would be if they lean democrat/ republican or if theyre independent


## c.) 
C_LR <- glm(K ~ Demo + Repo, data = survey)
summary(C_LR)
#=> DemoTRUE = 0.005 meaning that a person who is democrat its predicted they will answer
# 0.5% more accurately to if article was false or True
#=> RepoTRUE = -0.01 meaning that if person is republican its predicted they will answer
# -10% accurately to if article false or true

## d.)
# adding variables to control for Age, MediaMinutesPerDAy
D_LR <- glm(K ~ Demo + Repo + Age + MediaMinutesPerDay, data = survey)
summary(D_LR)




####### Question 10.)
### a.)
# Variable B = 1 if Yes, 0.5 if Not sure, 0 if no
survey$B <- ifelse(survey$ThoughtTrue == "Yes",1,ifelse(survey$ThoughtTrue == "Not sure", 0.5,0))

### b.)
B_LR <- glm(B ~ Demo + Repo, data = survey)
summary(B_LR)
# DemoTRUE = -0.033 meaning its predicted that if they are democrat they are -3.3% less
# likely to think any article is true
# RepoTRUE= 0.013 menaing its predicted if person is republican they are 1.3% more likely
# to think any article is true

### c.)
# Including interaction terms
Interact_LR <- glm(B ~  Demo*ArticleProClinton + Repo*ArticleProTrump, data = survey)
summary(Interact_LR)
# DemoTRUE = -0.11 meaning its predicted that if they are democrat they are -11% less
# likely to think any article is true
# RepoTRUE= -0.08 menaing its predicted if person is republican they are -8% less likely
# to think any article is true
# DemoTRUE:ArticleProClinton = 0.153 + (-0.11)= 0.043 meaning its predicted if person is democrat
# and the article was ProClinton, they are 4.3% more likely to beleive it was true
# RepoTRUE:ArticleProTrump = 0.189 + (-0.08)= 0.109  meaning its predicted if person is republican
# and the article was ProTrump, they are 10.9% more likely to believe it was true


