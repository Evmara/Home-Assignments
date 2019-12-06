#####################################Assignment 1#############################################

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

###uncommon: STAIT_trait: minimum value is 3.50 (ID 18), also the lowest income is -4562 --> these ID will be removed

mydata <- data_sample_1
mydata <- mydata[-c(18, 49), ]

View (mydata)

###investigate structure of the data
str(mydata)

mydata %>% summary ()

#########creating model 1#########

model1 <- lm(pain ~ sex + age, data = mydata)

###displaying model 1
summary (model1)

###scatterplots
windows()
plot1 = mydata %>% ggplot() + aes(x = sex, y = pain) +
  geom_point() + geom_smooth(method = "lm")
plot2 = mydata %>% ggplot() + aes(x = age, y = pain) +
  geom_point() + geom_smooth(method = "lm")
grid.arrange(plot1, plot2, nrow = 1)

###check for outliers, Cook´s distance
windows()
ols_plot_cooksd_bar(model1)

###checking assumptions for model 1###

###check first model for normality of the residuals by plotting a QQ plot
windows()
model1 %>% plot(which = 2)
shapiro.test(residuals (model1))

describe (residuals(model1))

###check first model for linearity
windows()
residualPlots (model1)
ols_test_normality (model1)


###check first model for homogeneity
ncvTest(model1)
###plot
windows()
plot (x = model1, which = 3)


###check first model for multicollinearity
vif(model1)

###display model1
model1

summary (model1)
confint(model1)
lm.beta(model1)


#########creating model 2########

model2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = mydata)

summary (model2)

###Cook´s outliers for second model
windows()
ols_plot_cooksd_bar (model2)


###checking assumptions for model 2###

###check second model for normality of the residuals by plotting a QQ plot
windows()
model2 %>% plot(which = 2)
shapiro.test(residuals (model2))

describe (residuals(model2))


###check second model for linearity
windows()
residualPlots (model2)
ols_test_normality (model2)


###check second model for homogeneity
ncvTest(model2)
###plot
windows()
plot (x = model2, which = 3)

###check second model for multicollinearity
vif(model2)
###numbers for cortisol_serum and cortisol_saliva are pretty high --> removal of the highest (cortisol_saliva)

###creating a new model 2
model2new <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = mydata)

###display model2
model2new


###check for outliers, Cook´s distance
windows ()
ols_plot_cooksd_bar(model2new)

###checking assumptions for model 2 new###

###check second-new model for normality of the residuals by plotting a QQ plot
windows()
model2new %>% plot(which = 2)
shapiro.test(residuals (model2new))
describe (residuals(model2new))


###check second-new model for linearity
windows()
residualPlots (model2)

ols_test_normality (model2new)



###check first model for homogeneity
ncvTest(model2new)
###plot
windows ()
plot (x = model2new, which = 3)


###check first model for multicollinearity
vif(model2new)

###display model2new
model2new

summary (model2new)
confint(model2new)
lm.beta(model2new)


######comparison of the two models######
###r squared
summary (model1)$adj.r.squared
summary (model2new)$adj.r.squared

###anova
anova(model1, model2new)

###AIC
AIC(model1)
AIC (model2new)
