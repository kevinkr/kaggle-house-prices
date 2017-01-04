###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature a
# Basement - Drop some levels
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

# drop columns used in new vars
fullSet <- subset(fullSet, select = -c(BsmtQual,TotalBsmtSF,BsmtFinType1,BsmtFinSF1,LowQualFinSF))
