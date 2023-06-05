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

# plot of odds
or <- exp(coef(model3))
ci <- exp(confint(model3))

# Create a data frame
data <- data.frame(predictor = names(or), odds_ratio = or, lower_ci = ci[,1], upper_ci = ci[,2])
color <- c("(Intercept)" = "#009E73", "AB" = "#0072B2", "B" = "#0072B2", "O" = "#0072B2",
           "BLACK" = "#D55E00", "HISPANIC" = "#D55E00", "ASIAN" = "#D55E00", "OTHER" = "#D55E00",
           "ADV" = "#CC79A7", "ND" = "#CC79A7", "SC" = "#CC79A7")

data$category <- c("(Intercept)", "ABO", "ABO", "ABO", "ETHNICITY", "ETHNICITY", 
                   "ETHNICITY", "ETHNICITY", "EDUCATION", "EDUCATION", "EDUCATION")
# Plot the odds ratios
ggplot(data, aes(x = predictor, y = odds_ratio, ymin = lower_ci, ymax = upper_ci, color = category)) +
  geom_pointrange(alpha = 0.5) +
  scale_color_manual(values = cbp4, name = "PREDICTOR") +
  xlab("PREDICTOR LEVELS") +
  geom_hline(yintercept = 1.0, lwd = 1, colour = "#F0E442", alpha = 0.5) +
  ylab("ODDS RATIO")

# poisson regression
model8 <- glm(floor(WAIT_TIME) ~ ABO + ETHCAT_1 + EDUCATION_1 + GENDER + AGE, 
              data = train, na.action = na.omit, family = "poisson")

summary(model8)

mean(model8$residuals^2)

exp(confint(model8))
exp(model8$coefficients)


