# House Prices: Advanced Regression Techniques 
# Deadline March 1, 2017

options(scipen=999) # remove scientific notation

library(dtplyr)

#Load data
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}

housing_data.path <- "https://raw.githubusercontent.com/kevinkr/kaggle-house-prices/master/Data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA","")

train.column.types <- c('integer',   # Id
                        'factor' ,   # MSSubClass
                        'factor',    # MSZoning
                        'numeric',   # LotFrontage
                        'numeric',   # LotArea
                        'factor',    # Street
                        'character',    # Alley
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
                        'character',    # BsmtQual (ordinal)
                        'character',    # BsmtCond (ordinal)
                        'character',    # BsmtExposure (ordinal)
                        'character',    # BsmtFinType1 (ordinal)
                        'numeric',   # BsmtFinSF1
                        'character',    # BsmtFinType2 (ordinal)                        
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
                        'character',    # FireplaceQu (ordinal)
                        'character',    # GarageType
                        'integer',   # GarageYrBlt
                        'character',    # GarageFinish
                        'integer',   # GarageCars
                        'numeric',   # GarageArea
                        'character',    # GarageQual (ordinal)
                        'character',    # GarageCond (ordinal) 
                        'factor',    # PavedDrive
                        'numeric',   # WoodDeckSF
                        'numeric',   # OpenPorchSF
                        'numeric',   # EnclosedPorch
                        'numeric',   # 3SsnPorch
                        'numeric',   # ScreenPorch
                        'numeric',   # PoolArea
                        'character',    # PoolQC (ordinal)
                        'character',    # Fence (ordinal)
                        'character',    # MiscFeature                        
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

#Examine data
str(train, list.len = 999) 
str(test, list.len = 999) 

# Bind test and train
test$SalePrice <- NA
test$isTest <- rep(1,nrow(test))
train$isTest <- rep(0,nrow(train))
fullSet <- rbind(test,train)

# set ordinal variables
fullSet$Alley[is.na(fullSet$Alley)] <- "No Alley Access"
fullSet$Alley <- factor(fullSet$Alley)
fullSet$OverallQual <- ordered(fullSet$OverallQual, levels = c(1,2,3,4,5,6,7,8,9,10))
fullSet$OverallCond <- ordered(fullSet$OverallCond, levels = c(1,2,3,4,5,6,7,8,9))
fullSet$ExterQual <- ordered(fullSet$ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$ExterCond <- ordered(fullSet$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))

fullSet$BsmtQual <- ordered(fullSet$BsmtQual, levels = c("No Bsmnt","Po","Fa","TA","Gd","Ex"))
fullSet$BsmtQual[is.na(fullSet$BsmtQual)] <- "No Bsmnt"

fullSet$BsmtCond <- ordered(fullSet$BsmtCond, levels = c("No Bsmnt","Po","Fa","TA","Gd","Ex"))
fullSet$BsmtCond[is.na(fullSet$BsmtCond)] <- "No Bsmnt"

fullSet$BsmtExposure<- ordered(fullSet$BsmtExposure, levels = c("No Bsmnt","No","Mn","Av","Gd"))
fullSet$BsmtExposure[is.na(fullSet$BsmtExposure)] <- "No Bsmnt"

fullSet$BsmtFinType1 <- ordered(fullSet$BsmtFinType1, levels = c("No Bsmnt","Unf","LWQ","Rec","BLQ","ALQ","GLQ"))
fullSet$BsmtFinType1[is.na(fullSet$BsmtFinType1)] <- "No Bsmnt"

fullSet$BsmtFinType2 <- ordered(fullSet$BsmtFinType2, levels = c("No Bsmnt","Unf","LWQ","Rec","BLQ","ALQ","GLQ"))
fullSet$BsmtFinType2[is.na(fullSet$BsmtFinType2)] <- "No Bsmnt"

fullSet$HeatingQC <- ordered(fullSet$HeatingQC, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$KitchenQual <- ordered(fullSet$KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"))
fullSet$Functional <- ordered(fullSet$Functional, levels = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"))

fullSet$FireplaceQu <- ordered(fullSet$FireplaceQu, levels = c("No Fplc","Po","Fa","TA","Gd","Ex"))
fullSet$FireplaceQu[is.na(fullSet$FireplaceQu)] <- "No Fplc"

fullSet$GarageType[is.na(fullSet$GarageType)] <- "No Garage"
fullSet$GarageType <- factor(fullSet$GarageType)

fullSet$GarageFinish <- ordered(fullSet$GarageFinish, levels = c("No Garage","Unf","RFn","Fin"))
fullSet$GarageFinish[is.na(fullSet$GarageFinish)] <- "No Garage"

fullSet$GarageQual <- ordered(fullSet$GarageQual, levels = c("No Garage","Po","Fa","TA","Gd","Ex"))
fullSet$GarageQual[is.na(fullSet$GarageQual)] <- "No Garage"

fullSet$GarageCond <- ordered(fullSet$GarageCond, levels = c("No Garage","Po","Fa","TA","Gd","Ex"))
fullSet$GarageCond[is.na(fullSet$GarageCond)] <- "No Garage"

fullSet$PavedDrive <- ordered(fullSet$PavedDrive, levels = c("N","P","Y"))

fullSet$PoolQC <- ordered(fullSet$PoolQC, levels = c("No Pool","Fa","TA","Gd","Ex"))
fullSet$PoolQC[is.na(fullSet$PoolQC)] <- "No Pool"

fullSet$Fence <- ordered(fullSet$Fence, levels = c("No Fence","MnWw","GdWo","MnPrv","GdPrv"))
fullSet$Fence[is.na(fullSet$Fence)] <- "No Fence"

fullSet$MiscFeature[is.na(fullSet$MiscFeature)] <- "None"
fullSet$MiscFeature <- factor(fullSet$MiscFeature)

# get var names for factors and numerics
cat.var <- names(train)[which(sapply(train, is.factor))]
num.var <- names(train)[which(sapply(train, is.numeric))]
num.var <- setdiff(num.var, c("Id", "SalePrice"))

train.cat <- train.raw[,cat.var]
train.num <- train.raw[,num.var]


