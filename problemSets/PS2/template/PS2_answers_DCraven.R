library(tidyverse)
install.packages("stargazer")
library(stargazer)
library(data.table)


data <- load("C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/climateSupport.RData")
# line 8 ########################################################################
str(data)
# chr [1:2] "climateSupport" ".Random.seed" These are the objects held
# in the RData file

str(climateSupport)
# 'data.frame':	8500 obs. of  3 variables:
# $ choice   : Factor w/ 2 levels "Not supported",..: 1 1 1 1 1 1 2 1 2 1 ...
# $ countries: Ord.factor w/ 3 levels "20 of 192"<"80 of 192"<..: 2 3 3 2 3 1
# $ sanctions: Ord.factor w/ 4 levels "None"<"5%"<"15%"<..: 3 3 1 3 2 3 2 4 2 

# Three columns, variables are of the data structure type 'factors'
# Check the order of factors
table(climateSupport$countries)
table(climateSupport$sanctions)
# line 23 ########################################################################
# Make factors unordered
climateSupport$countries <- factor(climateSupport$countries, ordered=FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered=FALSE)

# Rum GLM
glm <- glm(choice ~ ., data = climateSupport, family = binomial(link = "logit"))
summary(glm)
sum_glm <- summary(glm)

table_1 <- stargazer(sum_glm$coefficients, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/table_1.html")

# Reduced model
glm_red <- glm(choice ~ 1, data = climateSupport, family = binomial(link = "logit"))
sum_glm_red <- summary(glm_red)

test_null_hyp <- anova(glm_red, glm, test = "Chisq")
saveRDS(test_null_hyp, file="C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/test_null_hyp.Rda")

# Q2a
# How does baseline countries = 160 and sanctions 5% behave?
climateSupport2 <- copy(climateSupport)
climateSupport2$countries <- relevel(climateSupport2$countries, ref = "160 of 192")
climateSupport2$sanctions <- relevel(climateSupport2$sanctions, ref = "5%")
glm_2 <- glm(choice ~ ., data = climateSupport2, family = binomial(link = "logit"))
sum_glm_2 <- summary(glm_2)
sum_glm_2
# line 50 ########################################################################

table_2 <- stargazer(sum_glm_2$coefficients, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/table_2.html")


# Q2b
# How does baseline countries = 20 and sanctions 5% behave?
climateSupport3 <- copy(climateSupport)
climateSupport3$countries <- relevel(climateSupport3$countries, ref = "20 of 192")
climateSupport3$sanctions <- relevel(climateSupport3$sanctions, ref = "5%")
glm_3 <- glm(choice ~ ., data = climateSupport3, family = binomial(link = "logit"))
sum_glm_3 <- summary(glm_3)
sum_glm_3

table_3 <- stargazer(sum_glm_3$coefficients, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/table_3.html")


# # Q2c
# # Can either use original glm_model or 





# line 74 ########################################################################

# Probabilities for Q1a
exp(sum_glm$coefficients[1,1])/(1 + exp(sum_glm$coefficients[1,1]))
bl = sum_glm$coefficients[1,1]
exp(bl + sum_glm$coefficients[2,1])/(1 + exp(bl + sum_glm$coefficients[2,1]))
exp(bl + sum_glm$coefficients[3,1])/(1 + exp(bl + sum_glm$coefficients[3,1]))
exp(bl + sum_glm$coefficients[4,1])/(1 + exp(bl + sum_glm$coefficients[4,1]))
exp(bl + sum_glm$coefficients[5,1])/(1 + exp(bl + sum_glm$coefficients[5,1]))
exp(bl + sum_glm$coefficients[6,1])/(1 + exp(bl + sum_glm$coefficients[6,1]))

sum <- sum_glm$coefficients[1,1] + sum_glm$coefficients[2,1]
exp(sum)/(1 + exp(sum))

# line 88 ########################################################################

# Include interaction terms.
glm_int <- glm(choice ~ countries + sanctions + countries*sanctions, data = climateSupport, family = binomial(link = "logit") )
summ_glm_int <- summary(glm_int)
summary(glm_int)

table_4 <- stargazer(summ_glm_int$coefficients, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/table_4.html")

# Test for significance
test_int <- anova(glm, glm_int, test = "Chisq")
saveRDS(test_int, file="C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/test_int.Rda")

# Include interaction terms.
glm_int2 <- glm(choice ~ countries*sanctions, data = climateSupport, family = binomial(link = "logit") )
summ_glm_int2 <- summary(glm_int2)
summary(glm_int2)

table_5 <- stargazer(summ_glm_int2$coefficients, type = "html", out = "C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/table_5.html")

# Test for significance
test_int2 <- anova(glm, glm_int2, test = "Chisq")
saveRDS(test_int2, file="C:/Users/doire/Desktop/ASDS/Stats II/Problem sets/PS2/template/test_int2.Rda")
