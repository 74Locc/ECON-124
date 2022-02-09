## Eliseo Vega
## ECON-124
## Professor Leung
## January 8, 2022

#### Homework 1 ####
getwd()
setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1&2")
##setwd("/Users/eliseovega/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork1&2")
rm(list = ls())

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
#fakenews <- na.omit(fakenews)
x <- sum(na.omit(fakenews$fb_share[fakenews$pro == "Trump"]))
x

# y = number of times pro-Clinton articles were shared on Facebook
y <- sum(na.omit(fakenews$fb_share[fakenews$pro == "Clinton"]))
y

# Using cat() function to make a dynamic print message
# Multiplying x and y by 1000 since number of articles was in 1000s
cat(x*1000,"pro-Trump articles were shared on Facebook, and ", y*1000, "pro-Clinton articles were shared, in this dataset.")



######   Question 3   ######
#for each data base, 

# creating value 'Totalbuzz' for number of articles in fakenews 
Totalbuzz <- sum(fakenews$buzzfeed == 1)
# creating value 'Justbuzz' for buzz articles that are not in any other data base
Justbuzz <- Totalbuzz - sum(fakenews$buzzfeed == 1 & (fakenews$politifact == 1 | fakenews$snopes == 1))
# percent of buzz articles that dont appear in others
(Justbuzz / Totalbuzz)*100  #=42.9%

#Same Logic for Buzz operations above
Totalsnopes <- sum(fakenews$snopes == 1)
Justsnopes <- Totalsnopes - sum(fakenews$snopes == 1 & (fakenews$politifact == 1 | fakenews$buzzfeed == 1))
(Justsnopes / Totalsnopes)*100 #= 89.9%

#Same Logic for Buzz operations above
Totalpol <- sum(fakenews$politifact == 1)
Justpol <- Totalpol - sum(fakenews$politifact == 1 & (fakenews$snopes ==1 | fakenews$buzzfeed ==1))
(Justpol / Totalpol)*100 #= 69.2%



######   Question 4   ######
# Package to make numbers percentages
install.packages("formattable")
library(formattable)

AvgMinutes <- round(mean(survey$MediaMinutesPerDay),digits=0)
AvgMinutes
#average social media minute when media minutes was greater than 0
AvgSocialMinutes <- mean(survey$SocialMediaMinutesPerDay[survey$MediaMinutesPerDay > 0])
AvgSocialMinutes
a <- round((AvgSocialMinutes / AvgMinutes)*100,digits=0)
a

#using percent function with 0 allowable decimals
#SocialPercent <- percent(AvgSocialMinutes/AvgMinutes,0)

cat("Respondents reported spending on average", AvgMinutes, " minutes consuming
    election news, and ",a,"% of that time was spent on Social Media" )






######   Question 5   ######
#rep() creeates a 1 for the number of rows in survey, then tapply 
#tapply is using values of MostImport. and summing in that particular line
# storing this operation in the variable "z" so I can use in my barplot
z <- tapply(rep(1, nrow(survey)), survey$MostImportantSource, sum)
# making a barplot with sort() function on z to sort observations least to most
# using "las=2" to get the column names under each respective bar 
barplot(sort(z),las=2)








