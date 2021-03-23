
# Load the following required packages.

#library(sp)
#library(spatstat)
#library(maptools)
library(ggplot2)
library(MASS)
library(caret)

library(haven)
library(dplyr)
library(leaps)
library(tidyverse)

library(bootStepAIC)
library(MASS)
library(boot)
library(caret)
library(car)

# Set the working directory for your analyses

setwd("path to the directory that contains your dataset")


Data = read.csv("BokEfoul_Gamma0GF.csv")
Data[1:5,]

library(dplyr)


# Drop some columns from the data frame
Data2 = subset(Data, select = -c(PlotID, luse, no_of_gaps, gap_area))


#------------------------------------------------------------

#STEPWISE REGRESSION FOR THE SECOND SET OF datasets
# EXTRACTED SAR BACKSCATTER FROM 5 PIXEL MASKS OF DHP LOCATIONS IN EACH PLOT



pixelData = read.csv('BokEfoul_Gamma0GFpixels.csv')
View(pixelData)


library(dplyr)
# Drop some columns from the data frame
Data2p = subset(pixelData, select = -c(PlotID, Site, Year, X, no_of_gaps, gap_area))

View(Data2p)


#-------------------------------
# A stepwise regression methods in order to choose an optimal simple model, 
# without compromising the model accuracy.

library(caret)
library(dplyr)
library(lme4)

# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
# use 10-fold cross-validation to estimate the average prediction error (RMSE) of each of the models
train.control <- trainControl(method = "cv", number = 10)

#Data2b = mutate(Data2, logGF = log10(gap_fraction)) # transform gap fraction and store in column logGF
#Data2b = select(Data2b, -c(gap_fraction))

# Train the model
step.model <- train(gap_fraction~., data = Data2p,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE
)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)



pixelFinal_model = lm(gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, data = Data2p)
#logFinal_model = lm(logGF ~ Gamma0VH + Gamma0VVdb + VH.VVonVH.VV + VVVHratio + VV.VHonVV.VH, data = Data2b)

pixelFinal_model
#logFinal_model

summary(pixelFinal_model)
#summary(logFinal_model)



# To GET BETA COEFFICIENTS
#install.packages("lm.beta")

library(lm.beta)

pixelFinal_model.beta = lm.beta(pixelFinal_model)
print(pixelFinal_model.beta)
summary(pixelFinal_model.beta)
coef(pixelFinal_model.beta)



#par("mar")
par(mar=c(2.1,2.1,2.1,2.1))
#par(mar=c(5.1, 4.1, 4.1, 2.1)) # this is the diffault margin setting for par
# Visualise the model prediction and residuals
par(mfrow=c(2,2))

plot(pixelFinal_model)

par(mfrow=c(1,1))

plot(pixelFinal_model)


#--------------------------
#A1) STEPWISE REGRESSION WITH BOOTSTRAP FOR MODEL B (pixel based data)


pixelData = read.csv('BokEfoul_Gamma0GFpixels.csv')
View(pixelData)

library(dplyr)
# Drop some columns from the data frame
Data2p = subset(pixelData, select = -c(PlotID, Site, Year, X, no_of_gaps, gap_area))



# Fit the full model 
full.modelB <- lm(gap_fraction ~., data = Data2p)
summary(full.modelB)


#check fitted vs actual values

plot(full.modelB$model[,1], full.modelB$fitted.values,
     main = "Model B Actual vs Fitted values",
     pch=16, col = "indianred", cex=1.5,
     xlab = "Actual values", ylab = "Fitted values")
grid()
rstd = round(sd(full.modelB$residuals), 4)
legend("topleft",
       legend = paste("Residual std error = ", rstd), bty = 'n')


stepfitB = MASS::stepAIC(object = full.modelB, scope = list(upper=~., lower=~1), 
                         direction = "both", trace = F)
summary(stepfitB)
summary(full.modelB)


bootstepfitB = bootStepAIC::boot.stepAIC(object = full.modelB, data = Data2p, 
                                         B=999, alpha = 0.05, direction = "both", verbose = FALSE)
summary(bootstepfitB)
bootstepfitB



# BOOTSTRAP REGRESSION OF FINAL MODELS A AND B
#--------------------------------------------
#--------------------------------------------

# 1) MODEL B

bootReg = function (formula, data, indices)
{
  d = data[i,]
  fit = lm(formula, data=d)
  return(coe(fit))
}

modelB_fin = lm(gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, data = Data2p)

summary(modelB_fin)
library(lm.beta)
lm.beta(modelB_fin)
confint(modelB_fin)


install.packages("pastecs")
library(pastecs)
library(ggplot2)


#install.packages("DSUR")
# Computing the z scores for the dataframe (remove outliers with Z-scores >= 3)
library(matrixStats)
dfN_zscores = (Data2p - rowMeans(Data2p))/(rowSds(as.matrix(Data2p)))[row(Data2p)] 
View(dfN_zscores)


# Check for outliers and influencial cases
Data2p_diag = Data2p
Data2p_diag$residuals = resid(modelB_fin)
Data2p_diag$stdd.residuals = rstandard(modelB_fin)
Data2p_diag$cooks.dist = cooks.distance(modelB_fin)
Data2p_diag$dfbeta = dfbeta(modelB_fin)



# round up values for summary statistics
round(stat.desc(Data2p[, c("Gamma0VV", "Gamma0VVdb", "VVVHratio", "VHVVratio")], basic=FALSE, norm=TRUE), digits = 3)

#modelB_fin_Data = subset(Data2p, select = c(Gamma0VV, Gamma0VVdb, VVVHratio, VHVVratio))
#pairs(modelB_fin_Data)

#plot histogram
hist.VVdb = ggplot(Data2p, aes(Gamma0VVdb)) + options(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "VV backscatter (Gamma0 db)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2p$Gamma0VVdb, na.rm = TRUE), 
                                         sd(Data2p$Gamma0VVdb, na.rm = TRUE)), colour = "black", size = 1)

hist.VVdb

# The q-q (quantil-quantile) plot for variables in final model A
qqplot.VV = qplot(sample = Data2p$Gamma0VV, main = "q-q plot VV backscatter (Gamma0)")
qqplot.VVdb = qplot(sample = Data2p$Gamma0VVdb, main = "q-q plot VV backscatter (Gamma0 db)")
qqplot.VVVH = qplot(sample = Data2p$VVVHratio, main = "q-q plot VVVH backscatter ratio (Gamma0 db)")
qqplot.VHVV = qplot(sample = Data2p$VHVVratio, main = "q-q plot VHVV backscatter ratio (Gamma0 db)")
qqplot.VV
qqplot.VVdb
qqplot.VVVH
qqplot.VHVV

hist.VVVH = ggplot(Data2p, aes(VVVHratio)) + opt(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "VV-VH backscatter ratio (Gamma0 db)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2p$VVVHratio, na.rm = TRUE), 
                                         sd(Data2p$VVVHratio, na.rm = TRUE)), colour = "black", size = 1)


hist.VVVH


# Normality test for modelB_fin variables

shapiro.test(Data2p$Gamma0VV)
shapiro.test(Data2p$VVVHratio)


#by(pixelData$VVVHratio, pixelData$VVVHratio$Site, shapiro.test) # normality test by site

Aresiduals.ecdf = Ecdf(Data2p_diag$stdd.residuals)

Data2p_diag$large.residual = Data2p_diag$stdd.residuals > 2 | Data2p_diag$stdd.residuals < -2
sum(Data2p_diag$large.residual)

# MODEL DIAGNOSTICS
# Remove model outliers and cases with large standardized residuals (>2) based on ModelB_fin
Data2p_diag2 = Data2p_diag[Data2p_diag$stdd.residuals < 2, ]
names(Data2p_diag2)

#Data2p_diag2_cl = subset(Data2p_diag2, select = -c(residuals, stdd.residuals, cooks.dist, dfbeta, large.residual))

Data2p_diag2_cl = subset(Data2p_diag, select = -c(residuals, stdd.residuals, cooks.dist, dfbeta, large.residual))
names(Data2p_diag2_cl)

# C) Run model after removing influential cases or outliers (large residuals)


# C1 Fit a full model without removing cases with large residuals
full.modelB2 <- lm(gap_fraction ~., data = Data2p_diag2_cl)
summary(full.modelB)


#check fitted vs actual values

plot(full.modelB2$model[,1], full.modelB2$fitted.values,
     main = "Model B2 Actual vs Fitted values",
     pch=16, col = "indianred", cex=1.5,
     xlab = "Actual values", ylab = "Fitted values")
grid()
rstd = round(sd(full.modelB2$residuals), 4)
legend("topleft",
       legend = paste("Residual std error = ", rstd), bty = 'n')


stepfitB2 = MASS::stepAIC(object = full.modelB2, scope = list(upper=~., lower=~1), 
                         direction = "both", trace = F)
summary(stepfitB2)
summary(full.modelB2)


bootstepfitB2 = bootStepAIC::boot.stepAIC(object = full.modelB2, data = Data2p_diag2_cl, 
                                         B=999, alpha = 0.05, direction = "both", verbose = FALSE)
summary(bootstepfitB2)
summary(bootstepfitB2$OrigModel)
summary(bootstepfitB2$OrigStepAIC)
bootstepfitB2



#modelB2_fin = lm(gap_fraction ~ Gamma0VH + Gamma0VV + Gamma0VHdb + VVVHratio + 
#                   VHVVratio + VV.VHonVV.VH, data = Data2p_diag2_cl) # Model without large residual cases

modelB2_fin = lm(gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                 data = Data2p_diag2_cl) # Model with large residual cases (have acceptable cook's distances)
                   
summary(modelB2_fin)

# Run another model using only the significant variables based modelB2_fin

#modelB2_fin2 = lm(gap_fraction ~ Gamma0VV + Gamma0VHdb + VVVHratio + 
#                    VHVVratio + VV.VHonVV.VH, data = Data2p_diag2_cl) # without large residual cases (6)

modelB2_fin2 = modelB2_fin # model with large residuals included


summary(modelB2_fin2)
lm.beta(modelB2_fin2)
confint(modelB2_fin2)


#round(stat.desc(Data2p_diag2_cl[, c("Gamma0VV", "Gamma0VHdb", "VVVHratio", "VHVVratio", "VV.VHonVV.VH", "gap_fraction")], basic=FALSE, norm=TRUE), digits = 3)

# Diagnostics for distribution of variables in ModelB2

round(stat.desc(Data2p_diag2_cl[, c("Gamma0VV", "Gamma0VHdb", "VVVHratio", "VHVVratio", "gap_fraction")], basic=FALSE, norm=TRUE), digits = 3)


# Assess outliers and influential cases

Data2p_diag2_cl$residuals = resid(modelB2_fin2)
Data2p_diag2_cl$stdd.residuals = rstandard(modelB2_fin2)
Data2p_diag2_cl$cooks.dist = cooks.distance(modelB2_fin2)
Data2p_diag2_cl$dfbeta = dfbeta(modelB2_fin2)

# Plot Expected cummulative density function of standardized residuals
Aresiduals.ecdf = Ecdf(Data2p_diag2_cl$stdd.residuals) 

# Evaluate and count influential cases (with absolute standardized residuals > 2)

Data2p_diag2_cl$large.residual = Data2p_diag2_cl$stdd.residuals > 2 | Data2p_diag2_cl$stdd.residuals < -2
sum(Data2p_diag2_cl$large.residual)

# test assumption of independence model errors (durbinWatsonTest)
dwt(modelB2_fin2)

# Assess assumption of no multicolinearity

vif(modelB2_fin2)

# Check assumption about the residuals

plot(modelB2_fin2)



# ROBUST REGRESSION: bootstrapping for ModelB (since all assumptions for parametric model are violated)

# a) Bootstrap 95% CI for regression coefficients
bootReg_B2 = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(coef(fit))
}



library(boot)
#attach(Data2p_diag2_cl)

# Run Boostrap regression model B2 based on random x or case resampling
bootResultsB = boot(statistic = bootReg_B2, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                    data = Data2p_diag2_cl, R = 999) # bootstrap based on 999 replications


#View Bootstrap results
bootResultsB
plot(bootResultsB, index=1) # intercept
plot(bootResultsB, index=2) # Gamma0VV
plot(bootResultsB, index=3) # Gamma0VVdb
plot(bootResultsB, index=4) # VVVHratio
plot(bootResultsB, index=5) # VHVVratio
#plot(bootResultsB, index=6) # VV.VHonVV.VH

# Get 95% confidence intervals
boot.ci(bootResultsB, type = "bca", index=1) # intercept
boot.ci(bootResultsB, type = "bca", index=2) # Gamma0VV
boot.ci(bootResultsB, type = "bca", index=3) # Gamma0VVdb
boot.ci(bootResultsB, type = "bca", index=4) # VVVHratio
boot.ci(bootResultsB, type = "bca", index=5) # VHVVratio
#boot.ci(bootResultsB, type = "bca", index=6) # VV.VHonVV.VH


jack.after.boot(bootResultsB, index=2, main = "Gamma0VV") # Gamma0VV)
jack.after.boot(bootResultsB, index=3, main = "Gamma0VVdb") # Gamma0VHdb
jack.after.boot(bootResultsB, index=4, main = "VVVHratio") # VVVHratio
jack.after.boot(bootResultsB, index=5, main = "VHVVratio") # VHVVratio
#jack.after.boot(bootResultsB, index=6, main = "VV.VHonVV.VH") # VV.VHonVV.VH


# Another function to generate bootstrapped 95% confidence 
# interval for R-squared in the linear regresssion model

Rsq_B2 = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(summary(fit)$r.square)
}

bootRsqB2 = boot(statistic = Rsq_B2, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                data = Data2p_diag2_cl, R = 999) # bootstrap based on 999 replications

# View r-square results


bootRsqB2
plot(bootRsqB2)

# get 95% CI of r-square

boot.ci(bootRsqB2, type="bca")



# Another function to generate bootstrapped 95% confidence 
# interval for RMSE in the linear regresssion model

R_mse_B2 = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  #RSS = c(crossprod(fit$residuals))
  #MSE = RSS/length(fit$rediduals)
  Sq_MSE = sqrt(mean(fit$residuals^2))
  return(Sq_MSE)
}

bootRmse_2 = boot(statistic = R_mse_B2, formula = gap_fraction ~ Gamma0VV + Gamma0VVdb + VVVHratio + VHVVratio, 
                data = Data2p_diag2_cl, R = 999) # bootstrap based on 999 replications

# View rmse results


bootRmse_2
plot(bootRmse_2)
names(bootRmse_2)
View(bootRmse_2$t)
boxplot(bootRmse_2$t, main="Bootstrap RMSE Model B2")

# get 95% CI of rmse

boot.ci(bootRmse_2, type="bca")



#---------------------------------------
#---------------------------------------
#---------------------------------------

# 2) MODEL A


#bootReg = function (formula, data, indices)
#{
#  d = data[indices,]
#  fit = lm(formula, data=d)
#  return(coe(fit))
#}


# a) Evaluation ModelA for assumption of parametric tests

#modelA = gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio # DATA = Data2

modelA_fin = lm(gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, data = Data2)

summary(modelA_fin)
library(lm.beta)
lm.beta(modelA_fin)
confint(modelA_fin)


#install.packages("pastecs")
library(pastecs)
library(ggplot2)



# Checking Outliers in the data and assess homoskedasticity and multi-collinearity
# Computing the z scores for the dataframe (remove outliers with Z-scores >= 3)
library(matrixStats)
dfN_zscores_ModelA = (Data2 - rowMeans(Data2))/(rowSds(as.matrix(Data2)))[row(Data2p)] 
View(dfN_zscores_ModelA)


# Model diagnostics
# Check for outliers and influencial cases in the ModelA
Data2_diag = Data2
Data2_diag$residuals = resid(modelA_fin)
Data2_diag$stdd.residuals = rstandard(modelA_fin)
Data2_diag$cooks.dist = cooks.distance(modelA_fin)
Data2_diag$dfbeta = dfbeta(modelA_fin)


# Assess normality, skewness, etc. from descriptive statistics
# round up values for summary statistics
round(stat.desc(Data2[, c("Gamma0VH", "Gamma0VHdb", "Gamma0VVdb", "VVVHratio")], basic=FALSE, norm=TRUE), digits = 3)




#plot histograms to assess distribution of variables 
hist.VH_MA = ggplot(Data2, aes(Gamma0VH)) +  
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50) +
  labs(x = "ModelA VH backscatter (Gamma0)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2$Gamma0VH, na.rm = TRUE), 
                                         sd(Data2$Gamma0VH, na.rm = TRUE)), colour = "black", size = 1)

hist.VHdb_MA = ggplot(Data2, aes(Gamma0VHdb)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50) +
  labs(x = "ModelA VH backscatter (Gamm0 db)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2$Gamma0VHdb, na.rm = TRUE), 
                                         sd(Data2$Gamma0VHdb, na.rm = TRUE)), colour = "black", size = 1)

hist.VVdb_MA = ggplot(Data2, aes(Gamma0VVdb)) +  
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50) +
  labs(x = "ModelA VV backscatter (Gamma0 db)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2$Gamma0VVdb, na.rm = TRUE), 
                                         sd(Data2$Gamma0VVdb, na.rm = TRUE)), colour = "black", size = 1)

hist.VVVH_MA = ggplot(Data2, aes(VVVHratio)) +  
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 50) +
  labs(x = "ModelA VVVH backscatter ratio (Gamma0 db)", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data2$VVVHratio, na.rm = TRUE), 
                                         sd(Data2$VVVHratio, na.rm = TRUE)), colour = "black", size = 1)
hist.VH_MA 
hist.VHdb_MA
hist.VVdb_MA
hist.VVVH_MA

# Assess correlation between variables
modelA_fin_Data = subset(Data2, select = c(Gamma0VH, Gamma0VHdb, Gamma0VVdb, VVVHratio))
pairs(modelA_fin_Data, main="Scatter plot of Model A variables")
#install.packages("PerformanceAnalytics") #install package to get scatter plots with p values (use /N)

library(PerformanceAnalytics)
chart.Correlation(modelA_fin_Data, type="spearman", histogram = TRUE, pch=16, main="Model A variables") # plot correlation matrix with p-valu
#chart.Correlation(modelA_fin_Data, type="pearson", histogram = TRUE, pch=16, main="Model A variables")

rcorr(as.matrix(modelA_fin_Data), type="pearson")

library(corrplot)

cor(modelA_fin_Data, use = "complete.obs", method = c("pearson", "spearman", "kendall"))

# The q-q (quantil-quantile) plot for variables in final model A
qqplot.VH_MA = qplot(sample = Data2$Gamma0VH, main = "q-q plot VV backscatter (Gamma0VH)")
qqplot.VHdb_MA = qplot(sample = Data2$Gamma0VHdb, main = "q-q plot VV backscatter (Gamma0VHdb)")
qqplot.VVdb_MA = qplot(sample = Data2$Gamma0VVdb, main = "q-q plot VVVH backscatter (Gamma0VVdb)")
qqplot.VVVH_MA = qplot(sample = Data2$VVVHratio, main = "q-q plot VHVV backscatter ratio (VVVHratio)")
qqplot.VH_MA
qqplot.VHdb_MA
qqplot.VVdb_MA
qqplot.VVVH_MA



# Normality test for modelB_fin variables

shapiro.test(Data2$Gamma0VH)
shapiro.test(Data2$Gamma0VHdb)
shapiro.test(Data2$Gamma0VVdb)
shapiro.test(Data2$VVVHratio)


# Plot the expected cummulative density function of residuals of model A

Aresiduals.ecdf = Ecdf(Data2_diag$stdd.residuals, main="Expected CDF of ModelA Residuals")

# Identify cases with large residuals based on Model A 
Data2_diag$large.residual = Data2_diag$stdd.residuals > 2 | Data2_diag$stdd.residuals < -2
sum(Data2_diag$large.residual)
boxplot(Data2_diag$stdd.residuals)
boxplot(Data2_diag$cooks.dist)

View(Data2_diag[Data2_diag$large.residual ==TRUE,]) # view cases (5) with large residuals for Model A
#View(Data2p_diag[Data2p_diag$large.residual ==TRUE,]) # View cases (6) with large residuals for Model B

# MODEL DIAGNOSTICS
# Remove model outliers and cases with large standardized residuals (>2) based on ModelB_fin
#Data2_diag2 = Data2_diag[Data2_diag$stdd.residuals < 2, ]
#names(Data2_diag2)


# Subset only the needed data
#Data2_diag2_cl = Data2_diag[, c("residuals", "stdd.residuals", "cooks.dist", "dfbeta", "large.residual")]
Data2_diag2_cl = subset(Data2_diag, select = -c(residuals, stdd.residuals, cooks.dist, dfbeta, large.residual))
#Data2_diag2_cl = subset(Data2_diag2, select = -c(residuals, stdd.residuals, cooks.dist, dfbeta, large.residual))

names(Data2_diag2_cl)


# C) Run model after removing influential cases or outliers (large residuals)


# C1 Fit a full model 
full.modelA2 <- lm(gap_fraction ~., data = Data2_diag2_cl)
summary(full.modelA2)


#check fitted vs actual values

plot(full.modelA2$model[,1], full.modelA2$fitted.values,
     main = "Full Model A2 Actual vs Fitted values",
     pch=16, col = "indianred", cex=1.5,
     xlab = "Actual values", ylab = "Fitted values")
grid()
rstd = round(sd(full.modelA2$residuals), 4)
legend("topleft",
       legend = paste("Residual std error = ", rstd), bty = 'n')


stepfitA2 = MASS::stepAIC(object = full.modelA2, scope = list(upper=~., lower=~1), 
                          direction = "both", trace = F)
summary(stepfitA2)
summary(full.modelA2)

# Run the Bootstrap Stepwise Regression based random case/resampling with 999 replications
bootstepfitA2 = bootStepAIC::boot.stepAIC(object = full.modelA2, data = Data2_diag2_cl, 
                                          B=999, alpha = 0.05, direction = "both", verbose = FALSE)
summary(bootstepfitA2$OrigModel)
summary(bootstepfitA2$OrigStepAIC)
bootstepfitA2


# Create final model based on the results of boot.stepAIC (using selected variables)

modelA2_fin = lm(gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, data = Data2_diag2_cl) # Model with large residuals

#modelA2_fin = lm( gap_fraction ~ Gamma0VH + Gamma0VVdb + VH.VVonVH.VV + VVVHratio + VV.VHonVV.VH, data = Data2_diag2_cl) # Model withoug large residuals
summary(modelA2_fin)

#check fitted vs actual values

plot(modelA2_fin$model[,1], modelA2_fin$fitted.values,
     main = "Final Model A2 Actual vs Fitted values",
     pch=16, col = "indianred", cex=1.5,
     xlab = "Actual values", ylab = "Fitted values")
grid()
rstd = round(sd(modelA2_fin$residuals), 4)
legend("topleft",
       legend = paste("Residual std error = ", rstd), bty = 'n')

# Run another model using only the significant variables based modelA2_fin

modelA2_fin2 = lm(gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, data = Data2_diag2_cl) # with large residuals

#modelA2_fin2 = lm(gap_fraction ~ Gamma0VH + Gamma0VVdb + VVVHratio, data = Data2_diag2_cl) # without large residuals
summary(modelA2_fin2)
#check fitted vs actual values

plot(modelA2_fin2$model[,1], modelA2_fin2$fitted.values,
     main = "Final2 Model A2 Actual vs Fitted values",
     pch=16, col = "indianred", cex=1.5,
     xlab = "Actual values", ylab = "Fitted values")
grid()
rstd = round(sd(modelA2_fin2$residuals), 4)
legend("topleft",
       legend = paste("Residual std error = ", rstd), bty = 'n')


summary(modelA2_fin)
#summary(modelA2_fin2) # for model A2 with only significant predictors (3)
lm.beta(modelA2_fin)
#lm.beta(modelA2_fin2) # for model A2 with only significant predictors (3)
confint(modelA2_fin)
#confint(modelA2_fin2) # for model A2 with only significant predictors (3)


# Run diagnosis of the distribution of the variables in modelA2_fin2

round(stat.desc(Data2_diag2_cl[, c("Gamma0VH", "Gamma0VHdb", "Gamma0VVdb", "VVVHratio")], basic=FALSE, norm=TRUE), digits = 3)


# Assess outliers and influential cases

Data2_diag2_cl$residuals = resid(modelA2_fin)
Data2_diag2_cl$stdd.residuals = rstandard(modelA2_fin)
Data2_diag2_cl$cooks.dist = cooks.distance(modelA2_fin)
Data2_diag2_cl$dfbeta = dfbeta(modelA2_fin)

# Plot Expected cummulative density function of standardized residuals
Aresiduals.ecdf = Ecdf(Data2_diag2_cl$stdd.residuals) 

# Evaluate and count influential cases (with absolute standardized residuals > 2)

Data2_diag2_cl$large.residual = Data2_diag2_cl$stdd.residuals > 2 | Data2_diag2_cl$stdd.residuals < -2
sum(Data2_diag2_cl$large.residual)

View(Data2_diag2_cl[Data2_diag2_cl$large.residual == TRUE, ])

# test assumption of independence model errors (durbinWatsonTest)
dwt(modelA2_fin)

# Assess assumption of no multicolinearity

vif(modelA2_fin)






# ROBUST REGRESSION: bootstrapping for ModelA (since all assumptions for parametric model are violated)

# a) Bootstrap 95% CI for regression coefficients
bootRegA = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(coef(fit))
}



library(boot)

# Run bootstrap regression for the final model based on random or case resampling with 999 replication

bootResultsA = boot(statistic = bootRegA, formula = gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, 
                    data = Data2_diag2_cl, R = 999) # bootstrap based on 999 replications


#View Final Bootstrap Regreesion results
bootResultsA
plot(bootResultsA, index=1) # Boostrapp Model A intercept
plot(bootResultsA, index=2) # Boostrapp Model A Gamma0VH
plot(bootResultsA, index=3) # Boostrapp Model A Gamma0VHdb
plot(bootResultsA, index=4) # Boostrapp Model A Gamma0VVdb
plot(bootResultsA, index=5) # Boostrapp Model A VVVHratio


# Get 95% confidence intervals
boot.ci(bootResultsA, type = "bca", index=1) # intercept
boot.ci(bootResultsA, type = "bca", index=2) # Gamma0VH
boot.ci(bootResultsA, type = "bca", index=3) # Gamma0VHdb
boot.ci(bootResultsA, type = "bca", index=4) # Gamma0VVdb
boot.ci(bootResultsA, type = "bca", index=5) # VVVHratio



jack.after.boot(bootResultsA, index=2, main = "Boostrapp Model A2 Gamma0VH") # Gamma0VH
jack.after.boot(bootResultsA, index=3, main = "Boostrapp Model A2 Gamma0VHdb") # Gamma0VHdb
jack.after.boot(bootResultsA, index=3, main = "Boostrapp Model A2 Gamma0VVdb") # Gamma0VVdb
jack.after.boot(bootResultsA, index=4, main = "Boostrapp Model A2 VVVHratio") # VVVHratio



# Another function to generate bootstrapped 95% confidence 
# interval for R-squared in the linear regresssion model A

Rsq_MA2 = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(summary(fit)$r.square)
}

bootRsqA2 = boot(statistic = Rsq_MA2, formula = gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, 
                data = Data2_diag2_cl, R = 999) # bootstrap based on 999 replications

# View r-square results

bootRsqA2
plot(bootRsqA2, main="Bootstrap R-square Model A2")

# get 95% CI of r-square

boot.ci(bootRsqA2, type="bca")



# Another function to generate bootstrapped 95% confidence 
# interval for RMSE in the linear regresssion model

R_mse_MA2 = function (formula, data, indices) # function to obtain regression weights
{
  d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  #RSS = c(crossprod(fit$residuals))
  #MSE = RSS/length(fit$rediduals)
  Sq_MSE = sqrt(mean(fit$residuals^2))
  return(Sq_MSE)
}

bootRmse_MA2 = boot(statistic = R_mse_MA2, formula = gap_fraction ~ Gamma0VH + Gamma0VHdb + Gamma0VVdb + VVVHratio, 
                data = Data2_diag2_cl, R = 999) # bootstrap based on 999 replications

# View rmse results


bootRmse_MA2
plot(bootRmse_MA2)
names(bootRmse_MA2)
View(bootRmse_MA2$t)
boxplot(bootRmse_MA2$t, main="Bootstrap RMSE Model A2")

# get 95% CI of rmse

boot.ci(bootRmse_MA2, type="bca")


# Plotting the bootstrapped R-square for Model A2 (with large residuals)

R_sq_ModelA2_CI = boot.ci(bootRsqA2, type="bca")
summary(R_sq_ModelA2_CI)
CI_A2 = R_sq_ModelA2_CI$bca[,c(4,5)] # extract just the BCA CI values
print(CI_A2)  

hist(bootRsqA2$t, main = "Coefficient of Dertermination: Model A2", 
     xlab = "R-Squared", col = "grey", prob = T, breaks = seq(0.0, 0.7, 0.01))
lines(density(bootRsqA2$t), col = 'blue')
abline(v = CI_A2, col = 'red')



# Plotting the bootstrapped RMSE for Model A2

Rmse_ModelA2_CI = boot.ci(bootRmse_MA2, type="bca")
#summary(Rmse_ModelA2_CI)
RMSE_A2 = Rmse_ModelA2_CI$bca[,c(4,5)] # extract just the BCA CI values
print(RMSE_A2)  

hist(bootRmse_MA2$t, main = "RMSE : Model A2", 
     xlab = "rmse", col = "grey", prob = T, breaks = seq(5, 12, 0.1))
lines(density(bootRmse_MA2$t), col = 'blue')
abline(v = RMSE_A2, col = 'red')








# ---- install library for non-parametric correlation analysis ("Kendal") -----

#install.packages("Kendall")
library(Kendall)


# Plotting the bootstrapped R-square for Model B2 (with large residuals)

R_sq_ModelB2_CI = boot.ci(bootRsqB2, type="bca")
summary(R_sq_ModelB2_CI)
CI_B2 = R_sq_ModelB2_CI$bca[,c(4,5)] # extract just the BCA CI values
print(CI_B2)  

R_sq_ModelB2_CI
bootRsqB2

hist(bootRsqB2$t, main = "Coefficient of Dertermination: Model B2", 
     xlab = "R-Squared", col = "grey", prob = T, breaks = seq(0.0, 0.7, 0.01))
lines(density(bootRsqB2$t), col = 'blue')
abline(v = CI_B2, col = 'red')
  
  

# Plotting the For the bootstrapped RMSE for Model B2

Rmse_ModelB2_CI = boot.ci(bootRmse_2, type="bca")
summary(Rmse_ModelB2_CI)
RMSE_B2 = Rmse_ModelB2_CI$bca[,c(4,5)] # extract just the BCA CI values
print(RMSE_B2)  

bootRmse_2
Rmse_ModelB2_CI

hist(bootRmse_2$t, main = "RMSE : Model B2", 
     xlab = "rmse", col = "grey", prob = T, breaks = seq(5, 12, 0.1))
lines(density(bootRmse_2$t), col = 'blue')
abline(v = RMSE_B2, col = 'red')





