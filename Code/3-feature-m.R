###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature m
# OHE
library(dplyr)
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)

# OHE 
# use caret dummyVars function for hot one encoding for categorical features
library(dummies)
fullSet <- dummy.data.frame(fullSet, names=cat.var, sep="_")
colSums(sapply(fullSet, is.na)) > 0 # list of vars with missing values