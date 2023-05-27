# Reading in UNOS Data
# importing packages
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

# read in file
url <- 'file:///path to data/
UNOS%20data/Delimited%20Text%20File%20202206/
Kidney_%20Pancreas_%20Kidney-Pancreas/KIDPAN_DATA.htm'

fullTable <- readHTMLTable(url)
label <- fullTable$`NULL`[2]

KIDPAN_DATA <- read.delim("~/UNOS Data/Delimited Text File 202206/
                          Kidney_ Pancreas_ Kidney-Pancreas/KIDPAN_DATA.DAT", 
                          header=FALSE)

# combine headers and data
data <- rbind(t(label), KIDPAN_DATA)
names(data) <- data[1,]
data <- data[-1,]
rm(KIDPAN_DATA)

