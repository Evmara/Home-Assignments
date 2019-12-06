#load all necessary packages
library (psych)
library (tidyverse)
library (lm.beta)
library(gridExtra)
library (olsrr)
library (ggfortify)
library (car)
library(lme4) # for lmer
library(r2glmm) # for r2beta\t
library(MuMIn) # for r.squaredGLMM
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath
library (insight)

#insert data into R

data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv ")

#first look at the data
View (data_sample_3)

#descriptive statistics
describe (data_sample_3)
summary (data_sample_3)

#transform "Female" into "female"
mydata3 <- data_sample_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))

#deleting ID 195, since that person had 6.05 as a score for mindfulness (highest possible score would be 6.00)
mydata3 <- mydata3[-c(195),]

#checking
View(mydata3)

str(mydata3)

#linear mixed model
mod_rnd_int = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness +
                     ( 1 | hospital), data = mydata3, REML=FALSE)

###model diagnostics linear mixed model
#outliers
influence_observation = influence(mod_rnd_int, obs = T)$alt.fixed
# this can take a minute or so
influence_group = influence(mod_rnd_int, group = "hospital")$alt.fixed

#plotting
windows ()
data_plot_influence = as_tibble(influence_group) %>% gather(colnames(influence_group),
value = coefficient, key = predictor)
data_plot_influence %>% ggplot() + aes(x = 1, y = coefficient,
group = predictor) + geom_violin() + facet_wrap(~predictor,
scales = "free")

#normality
windows ()
qqmath(mod_rnd_int, id = 0.05)
#its fine, they roughly fit on a straight line.

#QQplot for the random effects
windows()
qqmath(ranef(mod_rnd_int))

#linearity
# scatterplot of the standardized residuals and the predicted values
windows ()
plot(mod_rnd_int, arg = "pearson")

#plotting predicted variable and fixed variables
mydata3 %>% ggplot() + aes(x = pain, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = sex, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = age, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = STAI_trait, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = pain_cat, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = cortisol_serum, y = residuals(mod_rnd_int)) +
  geom_point()

mydata3 %>% ggplot() + aes(x = mindfulness, y = residuals(mod_rnd_int)) +
  geom_point()

##Homoscedasticity
windows ()
plot(mod_rnd_int, arg = "pearson")
#Here, a funnel shape would indicate heteroscedasticity, but we do not see that in this plot.

homosced_mod = lm(residuals (mod_rnd_int)^2 ~ hospital, data = mydata3)
summary(homosced_mod)
#p-value: 0.4242 not significant so homoscedasticity is given. No need for further cyclone plots since its not even close to being significant

##multicollinearity
windows()
pairs.panels(mydata3[, c("sex", "age","STAI_trait", "pain_cat", "cortisol_serum","mindfulness")], col = "red", lm = T)
#all correlations are underneath the typical cutoff of 0.8, so multicollinearity is not given


# marginal R squared with confidence intervals
r2beta(mod_rnd_int, method = "nsj", data = mydata3)

#coefficients (under fixed effects)
summary (mod_rnd_int)

#mardinal R squared (fixed predictors) and conditional (fixed and ?? predictors) R squared
r.squaredGLMM(mod_rnd_int)

#confident intervalls
confint (mod_rnd_int)

##equation regression
#y = 3,41 + 0,30 * sexmale + (-0,06)*age + (-0,01)*STAI_trait + 0,08*pain_cat + 
#0,47 * cortisol_serum + (-0,18)*mindfulness

#retrieve data file 4
data_sample_4 = read.csv ("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

mydata4 <- data_sample_4

#describtive statistics
summary (mydata4)
#negative household_income is weird but we will keep it since the models to not include the variable of household income and the other variables of those participants seemed fine
View (mydata4)

#predict pain with equation of data file 3 on data file 4
pred_data_sample_4 <- predict(mod_rnd_int, data = mydata4, allow.new.levels =TRUE)
pred_data_sample_4


#RSS
RSS = sum((mydata4$pain - predict(mod_rnd_int, mydata4, allow.new.levels =TRUE))^2)

RSS

#TSS
mod_mean <- lmer(pain ~ 1 + (1 | hospital), data = mydata4)

error_plotter <- function(mod, col = "black", x_var = NULL) {
  mod_vars = as.character(mod$call[2])
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern = "~",
                                              mod_vars)) - 2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern = "~", mod_vars)) +
               2, nchar(mod_vars))
  data$pred = predict(mod)
  if (x == "1" & is.null(x_var)) {
    x = "response_ID"
    data$response_ID = 1:nrow(data)
  } else if (x == "1") {
    x = x_var
  }
  plot(data[, y] ~ data[, x], ylab = y, xlab = x)
  abline(mod)
  for (i in 1:nrow(data)) {
    clip(min(data[, x]), max(data[, x]), min(data[i, c(y,
                                                       "pred")]), max(data[i, c(y, "pred")]))
    abline(v = data[i, x], lty = 2, col = col)
  }
}

#plotting
windows()
error_plotter(mod_mean, col = "red", x_var = "cortisol_serum") # visualize error


TSS = sum((mydata4$pain - predict(mod_mean, mydata4, allow.new.levels = TRUE))^2)
TSS

R2 = 1 - (RSS/TSS)
R2

#variance components
get_variance_fixed (mod_rnd_int)
#random intercepts
get_variance_intercept(mod_rnd_int)
#residuals
get_variance_residual(mod_rnd_int)

#mardinal R squared and conditional R squared (needs to be compared with R^2 )
r.squaredGLMM(mod_rnd_int)

#neues lineares model --> highest influence variable
mod_rnd_int

#linear mixed model
mod_rnd_slope = lmer(pain ~ cortisol_serum +( cortisol_serum | hospital), data = mydata3)
mod_rnd_slope

#plotting
pred_slope = predict(mod_rnd_slope)

#how cortisol predicts pain in each hospital
windows()
mydata3 %>% ggplot() + aes(y = pain, x = cortisol_serum,
group = hospital) + geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) +
  facet_wrap(~hospital, ncol = 2)