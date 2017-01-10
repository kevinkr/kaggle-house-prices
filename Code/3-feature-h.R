###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature h
# Scale of continuous values
num.var <- sapply(fullSet, is.numeric)
fullSet[num.var] <- data.frame(lapply(fullSet[num.var], scale))

