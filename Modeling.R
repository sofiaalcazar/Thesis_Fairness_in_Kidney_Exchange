# modeling
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(aod)
library(pROC)
library(caret)
library(randomForest)
library(mlr)
library(MASS)
library(readxl)
library(XML)

# chi squared blood type
table_ABO <- table(subset_data_kidney$TRANSPLANT, subset_data_kidney$ABO)
chisq.test(table_ABO)

# chi squared ethnic category
table_ETH <- table(subset_data_kidney$TRANSPLANT, subset_data_kidney$ETHCAT_1)
chisq.test(table_ETH)

# chi squared education level
table_EDU <- table(subset_data_kidney$TRANSPLANT, subset_data_kidney$EDUCATION_1)
chisq.test(table_EDU)

# test/train split
set.seed(1)
split_1 <- sample(c(rep(0, 0.70 * nrow(subset_data_kidney)), 
                    rep(1, 0.30 * nrow(subset_data_kidney))))
train <- subset_data_kidney[split_1 == 0, ]
test <- subset_data_kidney[split_1 == 1, ]

# logistic regression
set.seed(3)

model3 <- glm(TRANSPLANT ~ ABO + ETHCAT_1 + EDUCATION_1,  
              data = train, family = binomial)


summary(model3)

exp(model3$coefficients)
exp(confint(model3))

predictions3 <- predict(model3, test, type = "response")

contrasts(test$TRANSPLANT)
test$classes3 <- ifelse(predictions3 > 0.5, "YES", "NO")

conf_matrix <- confusionMatrix(as.factor(test$classes3), test$TRANSPLANT)

conf_matrix$overall

# estimated marginal means
emmeans(model3, ~ABO, type = "response")
emmeans(model3, ~ETHCAT_1, type = "response")
emmeans(model3, ~EDUCATION_1, type = "response")

pairs(emmeans(model3, ~ABO, type = "response"))
pairs(emmeans(model3, ~ETHCAT_1, type = "response"))
pairs(emmeans(model3, ~EDUCATION_1, type = "response"))

# poisson regression
model8 <- glm(floor(WAIT_TIME) ~ ABO + ETHCAT_1 + EDUCATION_1 + GENDER + AGE, 
              data = train, na.action = na.omit, family = "poisson")

summary(model8)

mean(model8$residuals^2)

exp(confint(model8))
exp(model8$coefficients)


