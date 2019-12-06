#############################################Assignment 2##############################################

###load all necessary packages
library (psych)
library (tidyverse)
library (lm.beta)
library(gridExtra)
library (olsrr)
library (ggfortify)
library (car)


###insert data into R

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")

###first look at the data
View (data_sample_1)

###descriptive statistics
describe (data_sample_1)
summary (data_sample_1)

###uncommon: STAIT_trait: minimum value is 3.50 (ID 18), also the lowest income is -4562 (ID 49)

mydata <- data_sample_1
mydata <- mydata[-c(18, 49), ]

View (mydata)

###investigate structure of the data
str(mydata)

mydata %>% summary ()

###creating model for backwards regression###
modelall <-  lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + weight + IQ + household_income + mindfulness, data = mydata)

###view initial model
summary(modelall)


###check for outliers, Cook´s distance
windows ()
ols_plot_cooksd_bar(modelall)

###checking assumptions for initial model###

###check second model for normality of the residuals by plotting a QQ plot
windows()
modelall %>% plot(which = 2)
shapiro.test(residuals (modelall))
describe (residuals(modelall))


###check second model for linearity
windows()
residualPlots (modelall)
ols_test_normality (modelall)


###check second model for homogeneity
ncvTest(modelall)
###plot
windows()
plot (x = modelall, which = 3)

###check second model for multicollinearity
vif(modelall)

###display model1
modelall


###backwards regression
backwardmodel = step(modelall, direction = "backward")

###view backward model
backwardmodel <- backwardmodel
backwardmodel

###assumptions check for the backward model###

###check for outliers, Cook´s distance
windows ()
ols_plot_cooksd_bar(backwardmodel)


###check backward model for normality of the residuals by plotting a QQ plot
windows()
backwardmodel %>% plot(which = 2)
shapiro.test(residuals (backwardmodel))
describe (residuals(backwardmodel))


###check backward model for linearity
windows()
residualPlots (backwardmodel)
ols_test_normality (backwardmodel)


###check backward model for homogeneity
ncvTest(backwardmodel)
###plot
windows()
plot (x = backwardmodel, which = 3)


###check backward model for multicollinearity
vif(backwardmodel)


###R^2, adjusted R^2, F statistics (DF, p-value) t-values, p values
summary (backwardmodel)

###confidence intervalls
confint(backwardmodel)

###stndardizes coefficients
lm.beta(backwardmodel)

###display backward
backwardmodel


###running theorybasedmodel from the first assignments###
theorybasedmodel <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = mydata)


###comparing the two models - initial model & backward model
anova(modelall, backwardmodel)

###anova(backwardmodel, theorybasedmodel)
###not appropriate cause one of the models is not a subset of the other one


###AIC
AIC (modelall)
AIC (backwardmodel)
AIC (theorybasedmodel)

summary (modelall)
summary (backwardmodel)
summary (theorybasedmodel)

###getting the data
data_sample_2 = read.csv ("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

###having a quick look at the data
summary (data_sample_2)

###making predictions on pain 
pred_backwardmodel <- predict(backwardmodel, data_sample_2)
pred_theorybasedmodel <- predict (theorybasedmodel, data_sample_2)

sum((data_sample_2$pain - pred_backwardmodel)^2)
sum((data_sample_2$pain - pred_theorybasedmodel)^2) 
###pred_theorybasedmodel wins cause it has the lower residuals

