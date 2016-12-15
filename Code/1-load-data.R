# House Prices: Advanced Regression Techniques 
# Deadline March 1, 2017

options(scipen=999) # remove scientific notation

#Load data
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}

housing_data.path <- "https://raw.githubusercontent.com/kevinkr/kaggle-house-prices/master/Data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")

train.column.types <- c('integer',   # Id
                        'factor' ,   # MSSubClass
                        'factor',    # MSZoning
                        'numeric',   # LotFrontage
                        'numeric',   # LotArea
                        'factor',    # Street
                        'factor',    # Alley
                        'factor',    # LotShape (ordinal?)                       
                        'factor',    # LandContour
                        'factor',    # Utilities
                        'factor',    # LotConfig
                        'factor',    # LandSlope (ordinal?)
                        'factor',    # Neighborhood
                        'factor',    # Condition1
                        'factor',    # Condition2
                        'factor',    # BldgType
                        'factor',    # HouseStyle
                        'factor',    # OverallQual (ordinal)
                        'factor',    # OverallCond (ordinal)
                        'integer',   # YearBuilt
                        'integer',   # YearRemodAdd
                        'factor',    # RoofStyle
                        'factor',    # RoofMatl
                        'factor',    # Exterior1st
                        'factor',    # Exterior2nd
                        'factor',    # MasVnrType
                        'numeric',   # MasVnrArea
                        'factor',    # ExterQual (ordinal)
                        'factor',    # ExterCond (ordinal)
                        'factor',    # Foundation
                        'factor',    # BsmtQual (ordinal)
                        'factor',    # BsmtCond (ordinal)
                        'factor',    # BsmtExposure (ordinal)
                        'factor',    # BsmtFinType1 (ordinal)
                        'numeric',   # BsmtFinSF1
                        'factor',    # BsmtFinType2 (ordinal)                        
                        'numeric',   # BsmtFinSF2
                        'numeric',   # BsmtUnfSF
                        'numeric',   # TotalBsmtSF
                        'factor',    # Heating
                        'factor',    # HeatingQC (ordinal)
                        'factor',    # CentralAir
                        'factor',    # Electrical
                        'numeric',   # 1stFlrSF
                        'numeric',   # 2ndFlrSF
                        'numeric',   # LowQualFinSF
                        'numeric',   # GrLivArea
                        'integer',   # BsmtFullBath
                        'integer',   # BsmtHalfBath
                        'integer',   # FullBath
                        'integer',   # HalfBath
                        'integer',   # Bedroom
                        'integer',   # Kitchen
                        'factor',    # KitchenQual (ordinal)
                        'integer',   # TotRmsAbvGrd                     
                        'factor',    # Functional (ordinal)                        
                        'integer',   # Fireplaces
                        'factor',    # FireplaceQu (ordinal)
                        'factor',    # GarageType
                        'integer',   # GarageYrBlt
                        'factor',    # GarageFinish
                        'integer',   # GarageCars
                        'numeric',   # GarageArea
                        'factor',    # GarageQual (ordinal)
                        'factor',    # GarageCond (ordinal) 
                        'factor',    # PavedDrive
                        'numeric',   # WoodDeckSF
                        'numeric',   # OpenPorchSF
                        'numeric',   # EnclosedPorch
                        'numeric',   # 3SsnPorch
                        'numeric',   # ScreenPorch
                        'numeric',   # PoolArea
                        'factor',    # PoolQC (ordinal)
                        'factor',    # Fence (ordinal)
                        'factor',    # MiscFeature                        
                        'numeric',   # MiscVal
                        'integer',   # MoSold
                        'integer',   # YrSold
                        'factor',    # SaleType                       
                        'factor',    # SaleCondition                     
                        'numeric'    # SalePrice
)

test.column.types <- train.column.types[-81]     # # no SalePrice column in test.csv

train.raw <- readData(housing_data.path, train.data.file, 
                      train.column.types, missing.types)
train <- train.raw

test.raw <- readData(housing_data.path, test.data.file, 
                     test.column.types, missing.types)
test <- test.raw

head(train)
head(test)

# Bind test and train
test$SalePrice <- NA
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
fullSet <- rbind(test,train)

# set ordinal variables
fullSet$OverallQual <- ordered(fullSet$OverallQual, levels = c(1,2,3,4,5,6,7,8,9,10))
fullSet$OverallCond <- ordered(fullSet$OverallCond, levels = c(1,2,3,4,5,6,7,8,9))
fullSet$ExterQual <- ordered(fullSet$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$ExterCond <- ordered(fullSet$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$BsmtQual <- ordered(fullSet$BsmtQual, levels = c("NA","Po","Fa","TA","Gd","Ex"))
fullSet$BsmtCond <- ordered(fullSet$BsmtCond, levels = c("NA","Po","Fa","TA","Gd","Ex"))
fullSet$BsmtFinType1 <- ordered(fullSet$BsmtFinType1, levels = c("NA","Unf","LWQ","Rec","BLQ","ALQ","GLQ"))
fullSet$BsmtFinType2 <- ordered(fullSet$BsmtFinType2, levels = c("NA","Unf","LWQ","Rec","BLQ","ALQ","GLQ"))
fullSet$HeatingQC <- ordered(fullSet$HeatingQC, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$KitchenQual <- ordered(fullSet$KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$Functional <- ordered(fullSet$Functional, levels = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"))
fullSet$FireplaceQu <- ordered(fullSet$FireplaceQu, levels = c("NA","Po","Fa","TA","Gd","Ex"))
fullSet$GarageFinish <- ordered(fullSet$GarageFinish, levels = c("NA","Unf","RFn","Fin"))
fullSet$GarageQual <- ordered(fullSet$GarageQual, levels = c("NA","Po","Fa","TA","Gd","Ex"))
fullSet$GarageCond <- ordered(fullSet$GarageCond, levels = c("NA","Po","Fa","TA","Gd","Ex"))
fullSet$PavedDrive <- ordered(fullSet$PavedDrive, levels = c("N","P","Y"))
fullSet$PoolQC <- ordered(fullSet$PoolQC, levels = c("NA","Fa","TA","Gd","Ex"))
fullSet$Fence <- ordered(fullSet$Fence, levels = c("NA","MnWw","GdWo","MnPrv","GdPrv"))

# set factor levels all full set
library(dplyr)
fullSet <- fullSet %>% mutate_if(is.factor,is.factor)

# get var names for factors and numerics
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
num.var <- setdiff(num.var, c("Id", "SalePrice"))

# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(SalePrice))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))

rm(train.raw, test.raw, fullSet)
gc()
