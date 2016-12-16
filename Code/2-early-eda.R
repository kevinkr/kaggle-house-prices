# Kaggle House Prices, EDA

# load previous code
source("Code/1-load-data.R")

str(train)
str(test)

# Check for missing values
colSums(sapply(train, is.na))
colSums(sapply(test, is.na))

require(Amelia)
missmap(train, main="Train Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
# numerous missing values from PoolQC (NA=No Pool), MiscFeaure, Alley, Fence, FireplaecQu

missmap(test, main="Test Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)
# numerous missing values from LotFrontage, GarageYrBlt, MasVnrArea

library(Hmisc)    # for impute and some bystats

nonZeroVars <- colSums(sapply(fullSet, is.na)) >0

# Lot Frontage
sum(is.na(fullSet$LotFrontage))
fullSet$LotFrontage <- ifelse(is.na(fullSet$LotFrontage), mean(fullSet$LotFrontage, na.rm = TRUE), fullSet$LotFrontage)
# Utilities
sum(is.na(fullSet$Utilities))
table(fullSet$Utilities)
fullSet$Utilities[is.na(fullSet$Utilities)] <- "AllPub"
# Exterior1st
sum(is.na(fullSet$Exterior1st))
table(fullSet$Exterior1st)
fullSet$Exterior1st[is.na(fullSet$Exterior1st)] <- "VinylSd"
# Exterior2nd
sum(is.na(fullSet$Exterior2nd))
table(fullSet$Exterior2nd)
fullSet$Exterior2nd[is.na(fullSet$Exterior2nd)] <- "VinylSd"
# MasVnrType
sum(is.na(fullSet$MasVnrType))
table(fullSet$MasVnrType)
fullSet$MasVnrType[is.na(fullSet$MasVnrType)] <- "None"
# MasVnrArea
sum(is.na(fullSet$MasVnrArea))
fullSet$MasVnrArea <- ifelse(is.na(fullSet$MasVnrArea), mean(fullSet$MasVnrArea, na.rm = TRUE), fullSet$MasVnrArea)
# BsmtFinSF1
sum(is.na(fullSet$BsmtFinSF1))
fullSet$BsmtFinSF1 <- ifelse(is.na(fullSet$BsmtFinSF1), mean(fullSet$BsmtFinSF1, na.rm = TRUE), fullSet$BsmtFinSF1)
# BsmtFinSF2
sum(is.na(fullSet$BsmtFinSF2))
fullSet$BsmtFinSF2 <- ifelse(is.na(fullSet$BsmtFinSF2), mean(fullSet$BsmtFinSF2, na.rm = TRUE), fullSet$BsmtFinSF2)
# BsmntUnfSF
sum(is.na(fullSet$BsmtUnfSF))
fullSet$BsmtUnfSF <- ifelse(is.na(fullSet$BsmtUnfSF), mean(fullSet$BsmtUnfSF, na.rm = TRUE), fullSet$BsmtUnfSF)
# TotalBsmtSF
sum(is.na(fullSet$TotalBsmtSF))
fullSet$TotalBsmtSF <- ifelse(is.na(fullSet$TotalBsmtSF), mean(fullSet$TotalBsmtSF, na.rm = TRUE), fullSet$TotalBsmtSF)
# Electrical
sum(is.na(fullSet$Electrical))
table(fullSet$Electrical)
fullSet$Electrical[is.na(fullSet$Electrical)] <- "SBrkr"
# BsmtFullBath
sum(is.na(fullSet$BsmtFullBath))
table(fullSet$BsmtFullBath)
fullSet$BsmtFullBath[is.na(fullSet$BsmtFullBath)] <- 1
# BsmtHalfBath
sum(is.na(fullSet$BsmtHalfBath))
table(fullSet$BsmtHalfBath)
fullSet$BsmtHalfBath[is.na(fullSet$BsmtHalfBath)] <- 0

# KitchenQual
sum(is.na(fullSet$KitchenQual))
table(fullSet$KitchenQual)
fullSet$KitchenQual[is.na(fullSet$KitchenQual)] <- "TA"

# Functional
sum(is.na(fullSet$Functional))
table(fullSet$Functional)
fullSet$Functional[is.na(fullSet$Functional)] <- "Typ"
# GarageYrBlt
sum(is.na(fullSet$GarageYrBlt))
fullSet$GarageYrBlt <- ifelse(is.na(fullSet$GarageYrBlt), mean(fullSet$GarageYrBlt, na.rm = TRUE), fullSet$GarageYrBlt)

# GarageCars
sum(is.na(fullSet$GarageCars))
table(fullSet$GarageCars)
fullSet$GarageCars[is.na(fullSet$GarageCars)] <- 2

# GarageArea
sum(is.na(fullSet$GarageArea))
fullSet$GarageArea <- ifelse(is.na(fullSet$GarageArea), mean(fullSet$GarageArea, na.rm = TRUE), fullSet$GarageArea)

# SaleType
sum(is.na(fullSet$SaleType))
table(fullSet$SaleType)
fullSet$SaleType[is.na(fullSet$SaleType)] <- "WD"







# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(SalePrice))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))

rm(train.raw, test.raw)
gc()