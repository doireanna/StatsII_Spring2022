library(tidyverse)
library(nnet)
library(stargazer)
library(MASS)

setwd("C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template")
gdpChange <- read_csv("./gdpChange.csv")

head(gdpChange)

summary(gdpChange)

# Construct and interpret an unordered multinomial logit with GDPWdiff as the
# output and "no change" as the reference category, including the estimated 
# cutoff points and coefficients.

# Drop unnecessary rows
gdpChange <- gdpChange[,c("GDPWdiff", "REG", "OIL")]

# Just check if there are any zero values for "no change" category:
length(which(gdpChange$GDPWdiff==0))
# There are 16 zero entries for CGDWdiff

# Alter GDPWdiff to indicate "positive", "negative" or "no change".
# Bin GDPWdiff, will assign factors
gdpChange$GDPWdiff <- cut(gdpChange$GDPWdiff, 
                           breaks=c(min(gdpChange$GDPWdiff), -1, 0,
                                    max(gdpChange$GDPWdiff)),
                           labels=c("negative","no change","positive"))

# Check for N/A
unique(gdpChange$GDPWdiff)
# There is an N/A - find it:
which(is.na(gdpChange$GDPWdiff))
# Line 2221, remove it:
gdpChange <- gdpChange[-2221,]

# Set a reference level for the outcome
gdpChange$GDPWdiff <- relevel(gdpChange$GDPWdiff, ref = "no change")

# Run multinomial logit model
mult.log <- multinom(GDPWdiff ~ ., data = gdpChange)
sum_mult.log <- summary(mult.log)

# Get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2

# Create tables for 'negative' and 'positive' compared to 'no change'
tab_neg_mult.log <- rbind(sum_mult.log$coefficients[1,], 
                        sum_mult.log$standard.errors[1,], z[1,], p[1,])
rownames(tab_neg_mult.log) <- c("Coeff","Std. Errors","z stat","p value")


tab_pos_mult.log <- rbind(sum_mult.log$coefficients[2,], 
                        sum_mult.log$standard.errors[2,], z[2,], p[2,])
rownames(tab_pos_mult.log) <- c("Coeff","Std. Errors","z stat","p value")

# Stargazer:
table_1 <- stargazer(tab_neg_mult.log, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_1.html")

table_2 <- stargazer(tab_pos_mult.log, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_2.html")


# Interpret the coefficients:
exp(coef(mult.log))

table_3 <- stargazer(exp(coef(mult.log)), type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_3.html")


# We can use predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))
head(data.frame(GDPWdiff = gdpChange$GDPWdiff,
                no.change = pp$no.change,
                negative = pp$negative,
                positive = pp$positive))



# Generate data for prediction purposes:
predict_data2 <- data.frame(REG = rep(c(0,1), each = 2), OIL = rep(c(0,1), 2))

# Predict using new data
predicted_values2 <- cbind(predict_data2, predict(mult.log, newdata = predict_data2, type = "probs", se = TRUE))

# Mean probabilities:
(mean_probs2 <- by(predicted_values2[,3:5], predicted_values2$REG, colMeans))
(mean_probs3 <- by(predicted_values2[,3:5], predicted_values2$OIL, colMeans))



#line 92 #######################################################################
# Question 1b

# Construct an ordered multilogit  
# First, re-level factors
gdpChange$GDPWdiff <- relevel(gdpChange$GDPWdiff, "negative")

# Run ordered (proportional odds) logistic regression
ord.log <- polr(GDPWdiff ~ ., data = gdpChange, Hess = TRUE)
(sum_ord.log <- summary(ord.log))

# Calculate a p value
(ctable <- coef(summary(ord.log)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord.log))

# Convert to odds ratio
(final_table <- exp(cbind(OR = coef(ord.log), ci)))

table_6 <- stargazer(final_table, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_6.html")

#OR     2.5 %   97.5 %
#  REG 1.4884645 1.2851890 1.725834
#OIL 0.8258065 0.6590976 1.038771

#A unit change in REG increases the odds of GDP change achieving the leap into a higher category by a factor of e^0.3977=1.4884645.
#A unit change in OIL increases the odds of GDP change achieving the leap into a higher category by a factor of e^-0.1914=0.8258065.

install.packages("reshape")
library(reshape)

# Predict values
plot_data <- melt(cbind(predict_data2, predict(ord.log, predict_data2,
              type = "probs")), id.vars = c("REG", "OIL"),
              variable.name = "Level" , value.name = "Probability")

(mean(plot_data[1:4,4]))
(mean(plot_data[5:8,4]))
(mean(plot_data[9:12,4]))

(cut_points <- exp(ord.log$zeta))

table_9 <- stargazer(cut_points, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_9.html")





# line 142 Question 2#########################################################
install.packages("AER")
library(AER)
install.packages("pscl")
library(pscl)

MMD <- read_csv("./MexicoMuniData.csv")

# Remove unnecessary columns
MMD <- MMD[,c("PAN.visits.06", "competitive.district", "marginality.06",
              "PAN.governor.06")]

# Check for N/A
which(is.na(MMD))
# There are none

# Run Poisson regression
# First check if we need a zero-inflated model.
hist(MMD$PAN.visits.06)

MMD_poisson <- glm(MMD$PAN.visits.06 ~ ., data = MMD, family = poisson)
summary(MMD_poisson)

dt <- dispersiontest(MMD_poisson)
dt
mod.zip <- zeroinfl(PAN.visits.06 ~ ., data = MMD, dist = "poisson")
summary(mod.zip)

mod.zip_table <- cbind(mod.zip$coefficients$count, mod.zip$coefficients$zero)
colnames(mod.zip_table) <- c("Count", "Zip")

table_8 <- stargazer(mod.zip_table, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS3/template/table_8.html")

(expmod.zip_table <- exp(mod.zip_table))
(probmod.zip_table <- expmod.zip_table/(1 + expmod.zip_table))


# Provide the estimated mean number of visits from the winning PAN presidential
# candidate for a hypothetical district that was competitive
# (\texttt{competitive.district}=1), had an average poverty level 
# (\texttt{marginality.06} = 0), and a PAN governor 
# (\texttt{PAN.governor.06}=1).

data <- data.frame(PAN.visits.06 = 1, 
                   competitive.district = 1,
                   marginality.06 = 0,
                   PAN.governor.06 = 1)

predict(mod.zip, newdata = data, type = "zero", se = TRUE)
predict(mod.zip, newdata = data, type = "count", se = TRUE)
























