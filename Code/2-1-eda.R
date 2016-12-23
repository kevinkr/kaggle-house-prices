# Kaggle House Prices, EDA

# load previous code
#source("Code/1-load-data.R")

# Check for Missing Values ------------------------------------------------

colSums(sapply(train, is.na))
colSums(sapply(test, is.na))
colSums(sapply(fullSet, is.na)) > 0 # list of vars with missing values

# Resolve missing values --------------------------------------------------

# Lot Frontage
sum(is.na(fullSet$LotFrontage))
#fullSet$LotFrontage <- ifelse(is.na(fullSet$LotFrontage), mean(fullSet$LotFrontage, na.rm = TRUE), fullSet$LotFrontage)
# impute by neighborhood
library(Hmisc)    # for impute

list <- unique(fullSet$Neighborhood) 
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (i in var.levels) {
    impute.var[which(filter.var == i)] <- impute(impute.var[which(filter.var == i)])
  }
  return (impute.var)
}
fullSet$LotFrontage <- imputeMedian(fullSet$LotFrontage, fullSet$Neighborhood, list)

# Utilities
#sum(is.na(fullSet$Utilities))
#table(fullSet$Utilities)
fullSet$Utilities[is.na(fullSet$Utilities)] <- "AllPub"
# Exterior1st
#sum(is.na(fullSet$Exterior1st))
#table(fullSet$Exterior1st)
fullSet$Exterior1st[is.na(fullSet$Exterior1st)] <- "VinylSd"
# Exterior2nd
#sum(is.na(fullSet$Exterior2nd))
#table(fullSet$Exterior2nd)
fullSet$Exterior2nd[is.na(fullSet$Exterior2nd)] <- "VinylSd"
# MasVnrType
#sum(is.na(fullSet$MasVnrType))
#table(fullSet$MasVnrType)
fullSet$MasVnrType[is.na(fullSet$MasVnrType)] <- "None"
# MasVnrArea
#sum(is.na(fullSet$MasVnrArea))
#fullSet$MasVnrArea <- ifelse(is.na(fullSet$MasVnrArea), mean(fullSet$MasVnrArea, na.rm = TRUE), fullSet$MasVnrArea)
fullSet$MasVnrArea <- imputeMedian(fullSet$MasVnrArea, fullSet$Neighborhood, list)
# BsmtFinSF1
#sum(is.na(fullSet$BsmtFinSF1))
#fullSet$BsmtFinSF1 <- ifelse(is.na(fullSet$BsmtFinSF1), mean(fullSet$BsmtFinSF1, na.rm = TRUE), fullSet$BsmtFinSF1)
fullSet$BsmtFinSF1 <- imputeMedian(fullSet$BsmtFinSF1, fullSet$Neighborhood, list)

# BsmtFinSF2
#sum(is.na(fullSet$BsmtFinSF2))
#fullSet$BsmtFinSF2 <- ifelse(is.na(fullSet$BsmtFinSF2), mean(fullSet$BsmtFinSF2, na.rm = TRUE), fullSet$BsmtFinSF2)
fullSet$BsmtFinSF2 <- imputeMedian(fullSet$BsmtFinSF2, fullSet$Neighborhood, list)

# BsmntUnfSF
#sum(is.na(fullSet$BsmtUnfSF))
#fullSet$BsmtUnfSF <- ifelse(is.na(fullSet$BsmtUnfSF), mean(fullSet$BsmtUnfSF, na.rm = TRUE), fullSet$BsmtUnfSF)
fullSet$BsmtUnfSF <- imputeMedian(fullSet$BsmtUnfSF, fullSet$Neighborhood, list)

# TotalBsmtSF
#sum(is.na(fullSet$TotalBsmtSF))
#fullSet$TotalBsmtSF <- ifelse(is.na(fullSet$TotalBsmtSF), mean(fullSet$TotalBsmtSF, na.rm = TRUE), fullSet$TotalBsmtSF)
fullSet$TotalBsmtSF <- imputeMedian(fullSet$TotalBsmtSF, fullSet$Neighborhood, list)

# Electrical
#sum(is.na(fullSet$Electrical))
#table(fullSet$Electrical)
fullSet$Electrical[is.na(fullSet$Electrical)] <- "SBrkr"
# BsmtFullBath
#sum(is.na(fullSet$BsmtFullBath))
#table(fullSet$BsmtFullBath)
fullSet$BsmtFullBath[is.na(fullSet$BsmtFullBath)] <- 1
# BsmtHalfBath
#sum(is.na(fullSet$BsmtHalfBath))
#table(fullSet$BsmtHalfBath)
fullSet$BsmtHalfBath[is.na(fullSet$BsmtHalfBath)] <- 0

# KitchenQual
#sum(is.na(fullSet$KitchenQual))
#table(fullSet$KitchenQual)
fullSet$KitchenQual[is.na(fullSet$KitchenQual)] <- "TA"

# Functional
#sum(is.na(fullSet$Functional))
#table(fullSet$Functional)
fullSet$Functional[is.na(fullSet$Functional)] <- "Typ"
# GarageYrBlt
#sum(is.na(fullSet$GarageYrBlt))
#fullSet$GarageYrBlt <- ifelse(is.na(fullSet$GarageYrBlt), mean(fullSet$GarageYrBlt, na.rm = TRUE), fullSet$GarageYrBlt)
fullSet$GarageYrBlt <- imputeMedian(fullSet$GarageYrBlt, fullSet$Neighborhood, list)

# GarageCars
#sum(is.na(fullSet$GarageCars))
#table(fullSet$GarageCars)
fullSet$GarageCars[is.na(fullSet$GarageCars)] <- 2

# GarageArea
#sum(is.na(fullSet$GarageArea))
#fullSet$GarageArea <- ifelse(is.na(fullSet$GarageArea), mean(fullSet$GarageArea, na.rm = TRUE), fullSet$GarageArea)
fullSet$GarageArea <- imputeMedian(fullSet$GarageArea, fullSet$Neighborhood, list)

# SaleType
#sum(is.na(fullSet$SaleType))
#table(fullSet$SaleType)
fullSet$SaleType[is.na(fullSet$SaleType)] <- "WD"

# MSZoning
#sum(is.na(fullSet$MSZoning))
#table(fullSet$MSZoning)
fullSet$MSZoning[is.na(fullSet$MSZoning)] <- "RL"

###########################################
# Feature Engineering - Basement
# Drop some levels
fullSet <- subset(fullSet, select = -c(BsmtHalfBath,BsmtFinSF2,BsmtFinType2,BsmtFullBath, 
                                       BsmtCond, BsmtUnfSF, BsmtExposure))
# Create new features
fullSet$newBsmtQualSF <- as.numeric(factor(fullSet$BsmtQual, levels=c("No Bsmnt", "Po",
                                                                      "Fa", "TA", "Gd", "Ex")))
fullSet$newBsmtQualSF <- fullSet$newBsmtQualSF * fullSet$TotalBsmtSF

# Bsmt Fin Type 1
fullSet$newBsmtFinTypeSF <- as.numeric(factor(fullSet$BsmtFinType1, levels=c("No Bsmnt", "Unf",
                                                                             "LWQ", "Rec", "BLQ", "ALQ", "GLQ")))
fullSet$newBsmtFinTypeSF <- fullSet$newBsmtFinTypeSF * fullSet$BsmtFinSF1


# get var names for factors and numerics
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
num.var <- setdiff(num.var, c("Id", "SalePrice"))


# Adjust skewness in continuous ---------------------------------

#first get data type for each feature
 cont_nums <- c("LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF", 
                "TotalBsmtSF","FirstFlrSF","SecondFlrSF","LowQualFinSF","GrLivArea","GarageArea",
                "WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch","ScreenPorch","PoolArea",
                "MiscVal","newBsmtQualSF","newBsmtFinTypeSF")

# library(data.table)
# library(MASS)
# library(forecast)
# library(e1071)
# library(caret)
# library(forecast)

# fullSet <- setDT(fullSet)
# # remove skewness in train
# for (f in cont_nums) {
#   print(f)
#   tst <- e1071::skewness(fullSet[, eval(as.name(f))])
#   if (tst > .25) {
#     if (is.na(fullSet[, BoxCoxTrans(eval(as.name(f)))$lambda])) next
#     fullSet[, eval(as.name(f)) := BoxCox(eval(as.name(f)), BoxCoxTrans(eval(as.name(f)))$lambda)]
#   }
# }
# 
# # scale train
# for (f in cont_nums) {
#   fullSet[, eval(as.name(f)) := scale(eval(as.name(f)))]
# }


# Multicollinearity of numeric variables ---------------------------------------------
library(caret)
correlCutOff <- 0.80
#df = train[,(names(train) %in% num.var)]
#df = fullSet[,(names(fullSet) %in% num.var)] # use fullSet for analysis
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

#fullSet <- as.data.frame(fullSet)
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
# Remove insignificant categorical variables --------------------
######
# category reduction fucntion
######
# inputs category name, cutoff value
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(fullSet[[cat.name]])), decreasing = T)
  #return(proptable)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < cutoff.val])
  return(weak.prop.names)
}

############full loop attempt
for (n in cat.var) {
  #print(n)
  # call function to return category names for reduction, number is cutoff val
  #weak.prop.names <- reduce_cats(cat.name, 0.01)
  weak.prop.names <- reduce_cats(n, 0.03)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  fullSet[[n]] <- as.character(fullSet[[n]])
  fullSet[fullSet[[n]] %in% weak.prop.names, n] <- "OTHER"
  fullSet[[n]] <- as.factor(fullSet[[n]])
}


################# Zero or near zero variance
df = fullSet[,(names(fullSet) %in% cat.var)] # use fullSet for analysis
x <- nearZeroVar(df, saveMetrics = TRUE)

x[x[,"zeroVar"] > 0, ]
x[x[,"zeroVar"] + x[, "nzv"] > 0, ]

remaining <- x[which(x$nzv==FALSE),]
x[which(x$nzv==TRUE),]

