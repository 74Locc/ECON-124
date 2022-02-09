## Section Live

## IS-R^2 is how well your model fits and how robust it is for the data set you want 

## OOS-R^2 is how well your model fits for another data set. if it is changes alot
## this means that your model is not good for different data sets and model is really not that good

##What if you dont have a whole differnt data set to test OSS R^2 on the robustness of your model
## Cross-Validation: use this to get FOLDS of your data set and can treat them as differnt
## samples and run OOS for one of the samples
## Ex: if create 5 folds for a data set,
# you treat fold 1 for Out of Sammple, and other as IS 2,3,4,5
## then you go through each fold and treat as OOS for deviance 

## K-fold



## Regularization: Issue when you have 1000 x's and need to know what varibales should be included.
##                 More variables means more complex, less varibales mean less complex model

## the more varibles you have R^2 goes up means your model is great but what if varibales are useless
## adjusted r^2 gets rid of useless variables.

## This is the same concept with regularization.
## when you give lambda a penalty of how much it.
## get all the betas and assign to lambda_1 and call B^_1
## do that for all until lambda_100 and call B^100 and all each own model
## The higher the lambda the more complex and less Betas
#'*lambda_1 is the highest! most complex. All betas will be 0!!*
#'*lambda_100 is the lowest and least complex. All Betas included *
#'
## AICc is the sweetspot of what lambda to use when doing laso



## what to find what coeffiecient? which lambda do you want?
## coef ...... select=min


## something to know aftet to dooing gamlr
## what is ISR^2 =   1-((Y-Y^)^2/(Y-Ybar)^2)
## Y=Nba%salaary 
## Y^ = predict(.......,newdata=training)
## Ybar= mean(nba$salary)

## OOSR^2 is everything as above but just change newdata= test