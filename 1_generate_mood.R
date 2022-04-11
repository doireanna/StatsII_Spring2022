###############################################################################
#  CODE BY THE AUTHOR OF THE ORIGINAL STUDY, UNTIL OTHERWISE STATED ###########
###############################################################################



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
##header ##
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## setup  ----
rm(list=ls())

# check whether other packages than base packages are loaded
# if yes, they are detached to avoid
loadedpackages <- names(sessionInfo()$otherPkgs)
if (length(loadedpackages>0)) {
  lapply(paste("package",loadedpackages, sep=":"), detach, character.only = TRUE, unload = TRUE)
}

# define and set working directory
mywd <- "C:/Users/doire/Desktop/ASDS/Stats II/Replication study/Poisson model"
setwd(mywd)

# check if required folders exist
folders <- c("data","scripts","out")
all(sapply(folders, dir.exists))

# install required packages from CRAN
p_needed <- c("devtools","dplyr","emmeans","MASS")
# package versions used: devtools 2.4.3; dplyr 1.0.7; emmeans 1.7.1-1; MASS 7.3-54
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

# load packages
lapply(p_needed, require, character.only = TRUE)

# ggeffects automatically back-transforms values, even if this is unwanted, so slight adaptions had to be made to the ggemmeans-function
# this is a fork based on version 1.1.1.1 on Github
# here, I check if you already have the package installed and, if you do, will re-install the version you currently have at the end of the file
# CAUTION: if you installed from CRAN you will get the same version you had, if you installed from Github you will get the most recent version from there
# if you therefore need the old version from Github you have currently installed, do not install this
try(ggeffectsversion <- utils::packageDescription("ggeffects")$Version)
install_github("imrem/ggeffects", upgrade="never")
require(ggeffects)

rm(list=setdiff(ls(), "ggeffectsversion"))

load("data/applausebymonth.RDATA")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## cleaning ---- ## 

# AUSTRIA #
# after the BZÖ split from the FPÖ in 2005, both parties remained in the same PPG until the next election
# it therefore is not clear when government- and when opposition-MPs applauded, this period is removed entirely
applausebymonth <- applausebymonth[!(applausebymonth$country=="at" & 
                                       applausebymonth$date>="2005-04" & 
                                       applausebymonth$date<="2006-09"),]

# OK (ohne Klub, without PPG) are removed, as they are not really a PPG
applausebymonth <- applausebymonth[!(applausebymonth$party_from=="ohne Klubzugehörigkeit"),]
applausebymonth <- applausebymonth[!(applausebymonth$party_to=="ohne Klubzugehörigkeit"),]

# GENERAL #
# we are only interested in speeches by the gov parties
applausebymonth <- applausebymonth[applausebymonth$gov_party_to==1,]

# re-level the dyad-variable so that opposition applause to a government party is the reference category
applausebymonth$dyad2 <- relevel(applausebymonth$dyad2, ref = "gov-opp")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## model ----

# Calculate poisson model
p_model <- glm(applause ~ date*dyad2*country + 
                  words_party_to, family = 'poisson', data = applausebymonth)

###############################################################################
#  CODE BY THE DOIREANNA CRAVEN FROM HERE #####################################
###############################################################################


# Calculate negative binomial model
nb_model <- glm.nb(applause ~ date*dyad2*country + 
                     words_party_to, data = applausebymonth)
summary(nb_model)

#Residual plot for Poisson regression
p_res <- resid(p_model)
plot(fitted(p_model), p_res, col='steelblue', pch=16,
     xlab='Applause', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
nb_res <- resid(nb_model)
plot(fitted(nb_model), nb_res, col='steelblue', pch=16,
     xlab='Applause', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)

# Chi-squared test, likelihood ratio
p_value <- pchisq(2 * (logLik(nb_model) - logLik(p_model)), df = 1, lower.tail = FALSE)
p_value

# lrtest for likelihood ratio
library("lmtest")
lrtest(p_model, nb_model)

# Dispersion test
install.packages("AER")
library(AER)
dispersiontest(p_model)

# Distribution of applause
hist(applausebymonth$applause)

# Distribution of square root applause
hist(sqrt(applausebymonth$applause))

# Add square root of applause
applausebymonth$srapplause = sqrt(applausebymonth$applause)

# Calculate poisson model
srp_model <- glm(srapplause ~ date*dyad2*country + 
                 words_party_to, family = 'poisson', data = applausebymonth)

# Calculate negative binomial model
srnb_model <- glm.nb(srapplause ~ date*dyad2*country + 
                     words_party_to, data = applausebymonth)

#Residual plot for Poisson regression
srp_res <- resid(srp_model)
plot(fitted(srp_model), srp_res, col='steelblue', pch=16,
     xlab='Square root of Applause', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
srnb_res <- resid(srnb_model)
plot(fitted(srnb_model), srnb_res, col='steelblue', pch=16,
     xlab='Square root of Applause', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)

# Chi squared to check likelihood ratio
srp_value <- pchisq(2 * (logLik(srnb_model) - logLik(srp_model)), df = 1, lower.tail = FALSE)
srp_value

# lrtest to check likelihood ratio
lrtest(srp_model, srnb_model)

# Dispersion test
dispersiontest(srp_model)

# Look at log of applause
hist(log(applausebymonth$applause))

# Add log of applause
applausebymonth$logapplause = log(applausebymonth$applause)

# Subset on positive log terms
pos_log_applausebymonth = subset(applausebymonth, logapplause>=0)

# Calculate poisson model
logp_model <- glm(logapplause ~ date*dyad2*country + 
                   words_party_to, family = 'poisson', data = pos_log_applausebymonth)

# Calculate negative binomial model
lognb_model <- glm.nb(logapplause ~ date*dyad2*country + 
                       words_party_to, data = pos_log_applausebymonth)

#Residual plot for Poisson regression
logp_res <- resid(logp_model)
plot(fitted(logp_model), logp_res, col='steelblue', pch=16,
     xlab='Log of Applause', ylab='Standardized Residuals', main='Poisson')
abline(0,0)

#Residual plot for negative binomial regression 
lognb_res <- resid(lognb_model)
plot(fitted(lognb_model), lognb_res, col='steelblue', pch=16,
     xlab='Log of Applause', ylab='Standardized Residuals', main='Negative Binomial')
abline(0,0)

# Chi dquared for likelihood ratio
logp_value <- pchisq(2 * (logLik(lognb_model) - logLik(logp_model)), df = 1, lower.tail = FALSE)
logp_value

# lrtest for likelihood ratio
lrtest(logp_model, lognb_model)

# Dispersion test
dispersiontest(logp_model)

