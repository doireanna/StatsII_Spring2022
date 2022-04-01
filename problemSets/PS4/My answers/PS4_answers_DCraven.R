install.packages("eha")
install.packages("ggfortify")
library(eha)
library(tidyverse)
library(survival)
library(ggfortify)
library(stargazer)

# We're interested in modeling the historical causes of infant mortality.
# We have data from 5641 first-born in seven Swedish parishes 1820-1895. 
# Using the "infants" dataset in the eha library, fit a Cox Proportional
# Hazard model using mother's age and infant's gender as covariates.
# Present and interpret the output.

# The infant dataset:
data(infants)

# The response variable is `exit`
# Explanatory variables are:
# - age
# - sex

# First build a survival object:
infants_surv <- with(infants, Surv(enter, exit, event))
# 'with' evaluates an R expression in an environment constructed from data
# 'surv' returns the hazard function, which is the failure rate at time t, 
# conditional on a person having survived to that time

km_infants <- survfit(infants_surv ~ 1, data = infants)
# 'survfit' creates survival curves from the survival model
summary(km_infants, times = seq(0, 365, 5))
autoplot(km_infants)

# Now for the Cox Proportional Hazard regression
cox_infants <- coxph(infants_surv ~ sex + age, data = infants)
sum_cox_infants <- summary(cox_infants)
sum_cox_infants
stargazer(cox_infants, type = "text")
table_1 <- stargazer(cox_infants, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS4/table_1.html")

# The expected log of the hazard for male infants decreases by 0.49 compared
# to female infants, holding mother's age constant.
# The expected log of the hazard for infants decreases by 0.04 for every 
# additional year of mother's age, holding infant's sex constant. 

# Exponentiate parameter estimates to obtain hazard ratios:

# The hazard for male infants is 0.62 times the hazard for female babies, i.e.,
# female infant deaths are 38% lower than male infant deaths, holding mother's
# age constant at its mean.

# The hazard for infants is 4% lower for every extra year of mother's age, 
# holding sex constant at its mean.


