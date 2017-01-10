###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature l
# Reduce multicollinearity
# Multicollinearity of numeric variables ---------------------------------------------
library(caret)
correlCutOff <- 0.90
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
df <- subset(fullSet, select = num.var)
df2 = fullSet[,!(names(fullSet) %in% num.var)]
descrCorr <- cor(df)
descrCorr <- cor(fullSet[,(names(fullSet) %in% num.var)])
highCorr <- findCorrelation(descrCorr, correlCutOff)
colnames(df[,highCorr])
# remove highly correlated  continuous variables
if (length(highCorr) > 0) {
  df <- df[, -highCorr]
  fullSet <- cbind(df2,df)
}