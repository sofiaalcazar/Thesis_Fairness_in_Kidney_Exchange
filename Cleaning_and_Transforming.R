# Pre-processing and cleaning data
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

# subset of necessary fields
subset_data_kidpan <- data[, c('TRR_ID_CODE', 'WL_ID_CODE', 'AGE', 'ORGAN', 
                               'WL_ORG', 'ETHCAT', 'EDUCATION', 'ABO', 
                               'GSTATUS_KI', 'GENDER', 'ETHNICITY', 
                               'TX_DATE', 'INIT_DATE')]

# subset of only kidney data
subset_data_kidney <- subset_data_kidpan[subset_data_kidpan$ORGAN == 'KI' | 
                                           subset_data_kidpan$WL_ORG == 'KI', ]
rm(subset_data_kidpan)


# setting datatypes and combining factor levels
subset_data_kidney$AGE <- as.numeric(subset_data_kidney$AGE)
# make age null for children
subset_data_kidney$AGE[subset_data_kidney$AGE < 18] <- "NA"

# making blood type only A, B, AB, O
subset_data_kidney$ABO[subset_data_kidney$ABO == 'A1'] <- "A"
subset_data_kidney$ABO[subset_data_kidney$ABO == 'A2'] <- "A"
subset_data_kidney$ABO[subset_data_kidney$ABO == 'A1B'] <- "AB"
subset_data_kidney$ABO[subset_data_kidney$ABO == 'A2B'] <- "AB"
subset_data_kidney$ABO <- as.factor(subset_data_kidney$ABO)

subset_data_kidney$TX_DATE <- as.Date(subset_data_kidney$TX_DATE, 
                                      format = '%m/%d/%Y')
subset_data_kidney$INIT_DATE <- as.Date(subset_data_kidney$INIT_DATE, 
                                        format = '%m/%d/%Y')
subset_data_kidney$ETHNICITY <- as.factor(subset_data_kidney$ETHNICITY)
subset_data_kidney$GENDER <- as.factor(subset_data_kidney$GENDER)

# creating other education variable combining levels
subset_data_kidney$EDUCATION[subset_data_kidney$EDUCATION == '.'] <- "998"
subset_data_kidney$EDUCATION_1 <- subset_data_kidney$EDUCATION
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '1'] <- 'ND'
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '2'] <- 'ND'
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '3'] <- 'HS'
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '4'] <- 'SC'
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '5'] <- 'SC'
subset_data_kidney$EDUCATION_1[subset_data_kidney$EDUCATION_1 == '6'] <- 'ADV'
subset_data_kidney$EDUCATION_1 <- as.factor(subset_data_kidney$EDUCATION_1)
subset_data_kidney$EDUCATION <- as.factor(subset_data_kidney$EDUCATION)

# creating other variable with Amer Ind/Alaska Native, Native Hawaiian/other 
# Pacific Islander and multiracial
subset_data_kidney$ETHCAT_1 <- subset_data_kidney$ETHCAT
subset_data_kidney$ETHCAT_1[subset_data_kidney$ETHCAT_1 == '6'] <- "OTHER"
subset_data_kidney$ETHCAT_1[subset_data_kidney$ETHCAT_1 == '7'] <- "OTHER"
subset_data_kidney$ETHCAT_1[subset_data_kidney$ETHCAT_1 == '9'] <- "OTHER"
subset_data_kidney$ETHCAT_1 <- as.factor(subset_data_kidney$ETHCAT_1)
subset_data_kidney$ETHCAT <- as.factor(subset_data_kidney$ETHCAT)

# get rid of some 998/unknown values
subset_data_kidney <- subset_data_kidney[subset_data_kidney$ABO != "UNK", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$ETHCAT != "998", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$ETHCAT_1 != "998", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$ETHCAT_2 != "998", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$EDUCATION != "998", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$EDUCATION_1 != "998", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$EDUCATION != "996", ]
subset_data_kidney <- subset_data_kidney[subset_data_kidney$EDUCATION_1 != "996", ]

# creating transplant factor
subset_data_kidney$TRANSPLANT <- with(subset_data_kidney, 
                                      ifelse(TRR_ID_CODE == '' & 
                                               WL_ID_CODE != '', "NO", "YES"))
subset_data_kidney$TRANSPLANT <- as.factor(subset_data_kidney$TRANSPLANT)

# creating wait time variable
subset_data_kidney$WAIT_TIME <- difftime(subset_data_kidney$TX_DATE, subset_data_kidney$INIT_DATE, units = "weeks")
subset_data_kidney$WAIT_TIME <- as.numeric(subset_data_kidney$WAIT_TIME)

