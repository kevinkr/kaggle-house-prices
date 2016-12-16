# Kaggle House Prices, EDA

# load previous code
source("Code/1-load-data.R")

str(train)
str(test)

# Check for missing values
colSums(sapply(train, is.na))
colSums(sapply(test, is.na))
