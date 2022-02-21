# Eliseo Vega
# Feb 16, 2022
# ECON-124
# Prof. Lueng

##                          Homework 3                          ##

#############################     Question 1     ###############################
rm(list = ls())
getwd()
##setwd("/Users/elis_imac/Desktop/ECON-124_Winter_2022/Problem Sets/HomeWork3")
setwd("/Users/eliseovega/Desktop/ECON-124_Winter_2022/Problem Sets")

NY <- read.csv("NY_stop_frisk.csv")
names(NY)

#'*Changing Boro to factor, and labeling respective city of stop*
NY$BORO <- factor(NY$BORO, levels=c(1,2,3,4,5), labels = c("Manhattan","Bronx","Brooklyn",
                                                "Queens", "Staten Island"))
NY$BORO <- factor(NY$BORO)
                  
#'*Stopped inside or outside*
NY$INOUT <- factor(NY$INOUT,labels = c("In","Out"))
NY$INOUT <- factor(NY$INOUT)

#'*I.D. type of stopped person*
NY$TYPEOFID <- factor(NY$TYPEOFID, labels = c("Other","Photo","Refused","Verbal"))
NY$TYPEOFID <- factor(NY$TYPEOFID)

#'*Sex of Person*
NY$SEX <- factor(NY$SEX, labels = c("Female","Male","Other"))
NY$SEX <- factor(NY$SEX)

#'*Race*
NY$RACE <- factor(NY$RACE)
table(NY$RACE)

#'*Hair Color*
NY$HAIRCOLR <- factor(NY$HAIRCOLR)

#'*Eye color*
NY$EYECOLOR <- factor(NY$EYECOLOR)

#'*Suspects Build*
NY$BUILD <- factor(NY$BUILD)

#'*Offense*
NY$OFFCODE <- factor(NY$OFFCODE)

install.packages("gamlr")
library(gamlr)
library(Matrix)
NY <- naref(NY)
names(NY_sub)
str(NY_sub)


#############################     Question 2     ###############################

### a.)
#'*Ommiting variable Searched and that start with PF_ from data frame*
NY_sub <- subset(NY,select = -c(SEARCHED,PF_HANDS,PF_GRND,PF_WALL,PF_DRWEP,PF_PTWEP
                                ,PF_BATON,PF_HCUFF,PF_OTHER,PF_PEPSP))

#'*creating sparse matrix to predict frisking for all categorial variables*
Xmatrix <- model.matrix(FRISKED ~., data = naref(NY_sub))[,-1]
Xmatrix


#'*logit lasso, using 10-fold CV*
#'*Since dependent variable is binomial we have to set famil=binomial*
set.seed(7777)
logit_lasso <- cv.gamlr(Xmatrix, NY_sub$FRISKED, family="binomial", verb=TRUE, nfold = 10)
summary(logit_lasso)


### b.)
#'*Plot of lasso regularization path*
plot(logit_lasso$gamlr)
# The optimal penalty marked on the regularization path is lambda = 24 which is 
# very high and makes our model very complex. This model is complex.

#'*Plot of the cross-validation error against lambda*
plot(logit_lasso)

#'*Plot of AICc against lambda*
plot(log(logit_lasso$gamlr$lambda), AICc(logit_lasso$gamlr))

### c.)
#'*Seeing how sparse my vector of coefficient estimates *
cat("Total number of coefficients:", length(coef(logit_lasso)), "\n")

cat("number of non-zero coefficients for CV-optimal lambda:", length(which(coef(logit_lasso
                                                              ,select = "min")!=0)), "\n")
# 24 non-zero regressors are chosen by the optimal penalty. I would consider this model
# to have a very high complexity

cat("number of non-zero coefficients for AICc-optimal lambda:", length(which(coef(logit_lasso$gamlr)
                                                              !=0)), "\n")

### d.)
# The cross-validation error plot is unusual because it slopes down until lambda
# gives the least penalty and makes model the most complex. No according to my model
# so far there is no models that overfit.

### e.)
#'*Printing nonzero coefficients under optimal penalties*
optimal_model_coef <- coef(logit_lasso,select = "min")

optimal_model_coef
optimal_model_coef[which(optimal_model_coef!=0)]

# The 3 strongest predictors of frisking are:
# RF_FURT= 7.04, The strongest predictor of a frisk is furtive movement, 
#                which means moving a manner to no be noticed.
# RF_BULG= 6.82, The 2nd strongest predictor of a frisk is a suspicious bulge.
# RF_VCRIM= 6.78, The 3rd strongest predictor of a frisk is a violent crime suspected.

# Variables that were surprisingly not included were age and race.

#############################     Question 3     ###############################


### a.)
#'*Computing predicted probabilities for each observation*
pred <- drop(predict(logit_lasso$gamlr, Xmatrix, type= "response"))
hist(pred,)
# The shape of this plot has all of predictions either 0 or 1 which means the 
# algorithm is very uncertain.

### b.)
set.seed(0)
source("roc.R")


## IS-ROS Curve
par(mai=c(.9,.9,.2,.1))
roc(pred, NY_sub$FRISKED, bty="n", main= "IS ROC")

#'*Plotting cutoff of p =0.02*
points(x=1-mean((pred<=0.002)[NY_sub$FRISKED==0]), y=mean((pred>0.002)[NY_sub$FRISKED==1]) ,cex=1.5, pch=20, col='red')

#'*Plotting cutoff of p =0.1*
points(x=1-mean((pred<=0.1)[NY_sub$FRISKED==0]), y=mean((pred>0.1)[NY_sub$FRISKED==1]) ,cex=1.5, pch=20, col='blue')

#'*Plotting cutoff of p =0.33*
points(x=1-mean((pred<=0.33)[NY_sub$FRISKED==0]), y=mean((pred>0.33)[NY_sub$FRISKED==1]) ,cex=1.5, pch=20, col='black')

#'*Plotting cutoff of p =0.1*
points(x=1-mean((pred<=0.1)[NY_sub$FRISKED==0]), y=mean((pred>0.1)[NY_sub$FRISKED==1]) ,cex=1.5, pch=20, col='yellow')

#'*Plotting cutoff of p =0.1*
points(x=1-mean((pred<=0.1)[NY_sub$FRISKED==0]), y=mean((pred>0.1)[NY_sub$FRISKED==1]) ,cex=1.5, pch=20, col='orange')


legend("bottomright", fill=c("red","blue","black","yellow","orange"), legend=c("p = 0.02", "p = 0.1","p = 0.33", "p = 0.8", "p = 0.9"), bty="n",title="cutoffs")
###


### OOS ROC Curve
set.seed(0)
test <- sample.int(1000,500) #creating test set
#'*re-estimate model on training set and is why put [-test] since we dont want rows in this set*
logit_lasso_half <- gamlr(Xmatrix[-test,], NY_sub$FRISKED[-test], family = "binomial")

pred_oos <- predict(logit_lasso_half, Xmatrix[test,], type= "response") #predict on test set
Y_oos <- NY_sub$FRISKED[test] #outcomes of test set


roc(pred_oos, Y_oos, bty="n", main="OOS ROC")

#'* p = 0.02 *
points(x=1-mean((pred_oos<0.02) [Y_oos==0]), y=mean((pred_oos>0.02)[Y_oos==1]), cex=1.5, pch=20, col='red')

#'* p = 0.1 *
points(x=1-mean((pred_oos<0.1) [Y_oos==0]), y=mean((pred_oos>0.1)[Y_oos==1]), cex=1.5, pch=20, col='blue')

#'* p = 0.33 *
points(x=1-mean((pred_oos<0.33) [Y_oos==0]), y=mean((pred_oos>0.33)[Y_oos==1]), cex=1.5, pch=20, col='black')

#'* p = 0.8 *
points(x=1-mean((pred_oos<0.8) [Y_oos==0]), y=mean((pred_oos>0.8)[Y_oos==1]), cex=1.5, pch=20, col='yellow')

#'* p = 0.9 *
points(x=1-mean((pred_oos<0.9) [Y_oos==0]), y=mean((pred_oos>0.9)[Y_oos==1]), cex=1.5, pch=20, col='orange')

legend("bottomright", fill=c("red","blue","black","yellow","orange"), legend=c("p = 0.02", "p = 0.1","p = 0.33", "p = 0.8", "p = 0.9"), bty="n",title="cutoffs")
###

# The IS_ROC plot shows us that for all our selected probabilities, with exception to p = 0.02,
# we get a perfect fit with our model since all the chosen p's have a sensitivity of 1
# which means all the postive observations are classified correctly using this model.

# The OOS_ROC plot shows us that our model fits slightly worse since our points of p
# come closer to out 45degree line which is the line that is the worst possible case.

# This relates to the histogram in part a.) because it is shows all all the predictions are
# correctly classifying true positives and true negatives.



#### d.)

# If the NYPD were to get hold of my estimates and model, I would let them know of the 
# 24 non-zero estimators that are used to predict if a person is frisked. I would say
# that 24 estimators predicting if a random person can be frisked is arbitrary and subjective
# person to person.

# I would also note that since we can see that our OOS_ROC plot predicts slightly worse
# we know that with future data will predict frisking a person less accurately.
# This is especially important because future data will keep changing from the data the
# was used for this ML-model. I would suggest that this model constantly be updated with
# new data to keep the accuracy of frisking a person and taking away there 4th amendment.








