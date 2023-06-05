# Exploratory Data Analysis
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

# colors
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp2 <- c("#0072B2", "#D55E00", "#CC79A7", "#009E73", "#F0E442", "#999999")
cbp3 <- c("#0072B2", "#E69F00")
cbp4 <- c("#0072B2", "#D55E00", "#CC79A7", "#009E73", "#F0E442")

# bar chart of blood type
ggplot(subset_data_kidney, aes(x = ABO)) + 
  geom_bar(fill = "#0072B2", color = "white", width = 0.5, alpha = 0.5) + 
  labs(x = "ABO", y = "FREQUENCY") 

# bar chart of education level
ggplot(subset_data_kidney, aes(x = EDUCATION_1)) + 
  geom_bar(fill = "#0072B2", color = "white", width = 0.5, alpha = 0.5) + 
  labs(x = "EDUCATION", y = "FREQUENCY") + 
  scale_x_discrete(labels = c("GRADUATE DEGREE", "HIGH SCHOOL", 
                              "NO DEGREE", "SOME COLLEGE"))

# bar chart of ethnic category 
ggplot(subset_data_kidney, aes(x = ETHCAT_1)) + 
  geom_bar(fill = "#0072B2", color = "white", width = 0.5, alpha = 0.5) + 
  labs(x = "ETHCAT", y = "FREQUENCY") +
  scale_x_discrete(labels=c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER"))

# bar chart of transplant
ggplot(subset_data_kidney, aes(x = TRANSPLANT)) + 
  geom_bar(fill = "#0072B2", color = "white", width = 0.5, alpha = 0.5) + 
  labs(x = "TRANSPLANT", y = "FREQUENCY")

# histogram of waiting time
ggplot(subset_data_kidney, aes(x = WAIT_TIME)) + 
  geom_histogram(fill = "#0072B2", color = "white", alpha = 0.5) +
  labs(x = "WAIT TIME", y = "FREQUENCY") + 
  geom_vline(xintercept = mean(subset_data_kidney$WAIT_TIME, na.rm = TRUE), 
             lwd = 1, colour = "#D55E00", alpha = 0.5) +
  xlim(0, 600)

# wait time by ethnicity
ggplot(subset_data_kidney, aes(x = WAIT_TIME, fill = ETHCAT_1)) +
  geom_histogram(position = "fill", alpha = 0.5) +
  labs(x = "WAIT TIME", y = "PROPORTION") +
  scale_fill_discrete(name = "ETHCAT") +
  scale_fill_manual(values = cbp4, name  = "ETHNICITY", labels = c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER")) + 
  xlim(0, 600)

ggplot(subset_data_kidney, aes(x = ETHCAT_1, y = WAIT_TIME, fill = ETHCAT_1)) +
  geom_boxplot(alpha = 0.5) +
  labs(x = "ETHNICITY", y = "WAIT TIME") +
  scale_fill_manual(values = cbp4, name  = "ETHNICITY", labels = c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER")) +
  scale_x_discrete(labels = c("WHITE", "BLACK", "HISPANIC", "ASIAN", "OTHER")) +
  theme(legend.position = "none") +
  ylim(0, 1000)

# transplant by education level
ggplot(data = subset_data_kidney, aes(x = EDUCATION_1, fill = TRANSPLANT)) +
  geom_bar(position = "fill", alpha = 0.5) + 
  scale_fill_manual(values = cbp3, name = 'TRANSPLANT') + 
  labs(x= 'EDUCATION', y = 'PROPORTION') 

# transplant by blood type
ggplot(data = subset_data_kidney, aes(x = ABO, fill = TRANSPLANT)) +
  geom_bar(position = "fill", alpha = 0.5) + 
  scale_fill_manual(values = cbp3, name = 'TRANSPLANT') + 
  labs(x= 'ABO', y = 'PROPORTION') 

# transplant by blood type table
table(subset_data_kidney$TRANSPLANT, subset_data_kidney$ABO)

# transplant by ethnic category table
table(subset_data_kidney$TRANSPLANT, subset_data_kidney$ETHCAT_1)

# transplant by education level table
table(subset_data_kidney$TRANSPLANT, subset_data_kidney$EDUCATION_1)

# histogram of age
ggplot(subset_data_kidney, aes(x = AGE)) + 
  geom_histogram(binwidth = 5, fill = "#0072B2", color = "white", alpha = 0.5) +
  labs(x = "AGE", y = "FREQUENCY") +
  geom_vline(xintercept = mean(subset_data_kidney$AGE, na.rm = TRUE), 
             lwd = 1, colour = "#D55E00", alpha = 0.5)
