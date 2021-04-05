
# Load the following required packages.


# Set the working directory for your analyses

setwd("path to the directory that contains your dataset")


library(sp)
#library(spatstat)
#library(maptools)
library(ggplot2)
library(MASS)
library(caret)

#install.packages('leaps')
#install.packages('tidyverse')
#install.packages('dplyr')
#install.packages('haven')
#install.packages('bootStepAIC')

library(haven)
library(dplyr)
library(leaps)
library(tidyverse)

library(bootStepAIC)
library(MASS)
library(boot)
library(caret)
library(car)
#install.packages("finalfit")
library(finalfit)

setwd("path/to/the/folder/with/GapFractionData") # set the working directory


########################################################
#######################################################

# BOOTSTRAP REGRESSION OF FINAL MODELS A 
#--------------------------------------------
#--------------------------------------------


####################################
# 1) MODEL A Pixel-based Backscatter
###################################

library(caret)
library(dplyr)
library(lme4)


pixelData = read.csv('BokEfoul_Gamma0GFpixels.csv') # data extracted by 5 represenative pixels
View(pixelData)

# Drop some columns from the data frame
Data2p = subset(pixelData, select = -c(PlotID, Site, Year, X, no_of_gaps, gap_area))

View(Data2p)

#-----------------------------------------------
### SPLIT TRAINING AND TEST DATA

## 70% 0f the sample size
Data2p_smp = floor(.70 * nrow(Data2p))

## set seed to ensure data partition is reproducible
set.seed(84)

Training_ind = sample(seq_len(nrow(Data2p)), size = Data2p_smp) # get the rows for training dataset

Data2p_train = Data2p[Training_ind, ]  # subset of training data
Data2p_test = Data2p[-Training_ind, ]  # subset of test data

#-------------------------------------------

# CREATE THE FUNCTION FOR BOOSTRAP MODEL
# The inputs for the function: a regression formula using lm() function, a dataframe (data), 
# and a variable to referr to a particular bootstrap sample
bootReg = function (formula, data, indices){
  d = data[i,]
  fit = lm(formula, data=d)
  return(coe(fit))
}



#########################################
#############################

# ROBUST REGRESSION: bootstrapping for ModelA (since all assumptions for parametric model are violated)

##########################
########################################

# a) Bootstrap 95% CI for regression coefficients
# function to obtain regression weights
bootReg_model = function (formula, data, indices) {
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(coef(fit))
}


library(boot)
#attach(Data2p_diag2_cl)

# Run Boostrap regression model B2 based on random x or case resampling
bootResults = boot(statistic = bootReg_model, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                    data = Data2p_train, R = 999) # bootstrap based on 999 replications


#View Bootstrap results
bootResults


#----------------------------------------------
# Another function to generate bootstrapped 95% confidence 
# interval for R-squared in the linear regresssion model
#------------------------------------------------------

Rsq_model = function (formula, data, indices) {
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(summary(fit)$r.square) # return R-squared value from the regresssion model fit
}

bootRsqModel = boot(statistic = Rsq_model, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                 data = Data2p_train, R = 999) # bootstrap based on 999 replications


# View r-square results
bootRsqModel
plot(bootRsqModel)
summary(bootRsqModel)
summary(bootRsqModel, index=1)

# get 95% CI of r-square

boot.ci(bootRsqModel, type="bca")


#---------------------------------------------------
# Function to generate bootstrapped 95% confidence 
# interval for RMSE in the regresssion model
#----------------------------------------------------

R_mse_model = function (formula, data, indices) {
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  #RSS = c(crossprod(fit$residuals))
  #MSE = RSS/length(fit$rediduals)
  Sq_MSE = sqrt(mean(fit$residuals^2)) # compute the RMSE
  return(Sq_MSE)
}

bootRmseModel = boot(statistic = R_mse_model, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                  data = Data2p_train, R = 999) # bootstrap based on 999 replications

# View rmse results

bootRmseModel
plot(bootRmseModel)
names(bootRmseModel)
View(bootRmseModel$t)

library(Hmisc)
library(psych)
describe(bootRmseModel$t)
summary(bootRmseModel$t)
boxplot(bootRmseModel$t, main="Bootstrap RMSE Model B2")

# get 95% CI of rmse

boot.ci(bootRmseModel, type="bca")


#------------------------------
# Use boot_predict() function to get bootstrap prediction from test dataset
#-------------------------------

#install.packages("readr")
library(readr)
#install.packages("haven")
library(haven)
#install.packages("rio")
library(rio)
#install.packages("car")
library(car)



# Make dataframe for which we want predictions (bootstrap resampling of test dataset)
# Resample test dataset 999 times, and calculate the mean each time.
newData2p <- replicate(999, mean(sample(Data2p, replace=T)))

library(car)
fit = lm(gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, data = Data2p_train)


#------------------------------------------
# Boostrap Model Prediction Function
#------------------------------------------

Predict_M1 = function (formula, data, indices){
  d = data[indices,] # allows boot to select sample
  #d = data # do not allows boot to select sample
  fit = lm(formula, data=d)
  return(predict(fit))
}


PredtGF_M1 = boot(statistic = Predict_M1, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                  data = Data2p, R = 999) # bootstrap based on 999 replications of original dataset

PredtGF_M1
names(PredtGF_M1)
plot(PredtGF_M1)

summary(PredtGF_M1$t0)
sd(PredtGF_M1$t0)


# get 95% CI of prediction

boot.ci(PredtGF_M1, type="bca")



# ---- install library for non-parametric correlation analysis ("Kendal") -----

#install.packages("Kendall")
library(Kendall)


# Plotting the bootstrapped R-square for Model  

Rsq_model_CI = boot.ci(bootRsqModel, type="bca")
summary(Rsq_model_CI)
CI_B2 = Rsq_model_CI$bca[,c(4,5)] # extract just the BCA CI values
print(CI_B2)  

Rsq_model_CI
bootRsqModel

hist(bootRsqModel$t, main = "Coefficient of Dertermination: Model", 
     xlab = "R-Squared", col = "grey", prob = T, breaks = seq(0.0, 0.7, 0.01))
lines(density(bootRsqModel$t), col = 'blue')
abline(v = CI_B2, col = 'red')
  
  

# Plotting the bootstrapped RMSE for Model

Rmse_Model_CI = boot.ci(bootRmseModel, type="bca")
summary(Rmse_Model_CI)
RMSE_Mod = Rmse_Model_CI$bca[,c(4,5)] # extract just the BCA CI values
print(RMSE_Mod)  

bootRmseModel
Rmse_Model_CI

hist(bootRmseModel$t, main = "RMSE : Model B2", 
     xlab = "rmse", col = "grey", prob = T, breaks = seq(5, 12, 0.1))
lines(density(bootRmseModel$t), col = 'blue')
abline(v = RMSE_Mod, col = 'red')





