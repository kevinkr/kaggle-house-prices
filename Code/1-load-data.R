# House Prices: Advanced Regression Techniques 
# Deadline March 1, 2017

options(scipen=999) # remove scientific notation

# library(dtplyr)
# library(data.table)
 library(dplyr)
library(plyr)
# library(e1071)
# library(forecast)
# library(MASS)
# #library(caret)
# 
# library(ggplot2)

# Load data ---------------------------------------------------------------

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
                        'numeric',   # LotFrontage 1c
                        'numeric',   # LotArea 2c
                        'factor',    # Street
                        'character', # Alley
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
                        'integer',   # YearBuilt 1d
                        'integer',   # YearRemodAdd 2d
                        'factor',    # RoofStyle
                        'factor',    # RoofMatl
                        'factor',    # Exterior1st
                        'factor',    # Exterior2nd
                        'factor',    # MasVnrType
                        'numeric',   # MasVnrArea 3c
                        'factor',    # ExterQual (ordinal)
                        'factor',    # ExterCond (ordinal)
                        'factor',    # Foundation
                        'character', # BsmtQual (ordinal)
                        'character', # BsmtCond (ordinal)
                        'character', # BsmtExposure (ordinal)
                        'character', # BsmtFinType1 (ordinal)
                        'numeric',   # BsmtFinSF1 4c
                        'character', # BsmtFinType2 (ordinal)                        
                        'numeric',   # BsmtFinSF2 5c
                        'numeric',   # BsmtUnfSF 6c
                        'numeric',   # TotalBsmtSF 7c
                        'factor',    # Heating
                        'factor',    # HeatingQC (ordinal)
                        'factor',    # CentralAir
                        'factor',    # Electrical
                        'numeric',   # 1stFlrSF 8c
                        'numeric',   # 2ndFlrSF 9c
                        'numeric',   # LowQualFinSF 10c
                        'numeric',   # GrLivArea 11c
                        'integer',   # BsmtFullBath 3d
                        'integer',   # BsmtHalfBath 4d
                        'integer',   # FullBath 5d
                        'integer',   # HalfBath 6d
                        'integer',   # Bedroom 7d
                        'integer',   # Kitchen 8d
                        'factor',    # KitchenQual (ordinal)
                        'integer',   # TotRmsAbvGrd 9d          
                        'factor',    # Functional (ordinal)                        
                        'integer',   # Fireplaces 10d
                        'character', # FireplaceQu (ordinal)
                        'character', # GarageType
                        'integer',   # GarageYrBlt 11d
                        'character', # GarageFinish
                        'integer',   # GarageCars 12d
                        'numeric',   # GarageArea 12c
                        'character', # GarageQual (ordinal)
                        'character', # GarageCond (ordinal) 
                        'factor',    # PavedDrive
                        'numeric',   # WoodDeckSF 13c
                        'numeric',   # OpenPorchSF 14c
                        'numeric',   # EnclosedPorch 15c
                        'numeric',   # 3SsnPorch 16c
                        'numeric',   # ScreenPorch 17c
                        'numeric',   # PoolArea 18c
                        'character', # PoolQC (ordinal)
                        'character', # Fence (ordinal)
                        'character', # MiscFeature                        
                        'numeric',   # MiscVal 19c
                        'integer',   # MoSold 13d
                        'integer',   # YrSold 14d
                        'factor',    # SaleType                       
                        'factor',    # SaleCondition                     
                        'numeric'    # SalePrice 20c
)

test.column.types <- train.column.types[-81]     # # no SalePrice column in test.csv
train.raw <- readData(housing_data.path, train.data.file, 
                      train.column.types, missing.types)
train <- train.raw
test.raw <- readData(housing_data.path, test.data.file, 
                     test.column.types, missing.types)
test <- test.raw

neighborhoodNeighbors <- read.csv("Data/nieghborhood adj neighbors.csv",header = TRUE,sep=",", fileEncoding="UTF-8-BOM")
neighborhoodProfile <- ddply(train.raw, c("Neighborhood"), plyr::summarize,  meanNbrhd=mean(SalePrice), 
                             medianNbrhd=median(SalePrice))
neighborhoodNeighbors <- cbind(neighborhoodNeighbors, meanNbrhd=neighborhoodProfile$meanNbrhd)

# drop some outliers from train ----------------------------
# Remove outliers with LotArea bigger than 60000
lotOutliers <- which(with( train, LotArea > 60000 ))
lotOutliers
train <- train[ -lotOutliers, ]

# Remove outliers where SalePrice/TotRmsAbvGrd > 400
# salePriceOutliers < which(with( train, SalePrice/TotRmsAbvGrd > 400))
# salePriceOutliers
# fullSet <- fullSet[ -salePriceOutliers, ]

# Drop outlier entries where SecondFlrSF > 1800
secondFlrSFOutliers <- which(with( train, X2ndFlrSF > 1800 ))
secondFlrSFOutliers
train <- train[ -secondFlrSFOutliers, ]

# Drop outlier entries where FirstFlrSF > 2600
FirstFlrSFOutliers <- which(with( train, X1stFlrSF > 2600 ))
FirstFlrSFOutliers
train <- train[ -FirstFlrSFOutliers, ]

# Adjust categorical features ---------------------------------------------

# combine train and test data for preprocessing
fullSet <- rbind(dplyr::select(train,MSSubClass:SaleCondition),
                  dplyr::select(test,MSSubClass:SaleCondition))

# correct variable names changed by read.csv
names(fullSet)[names(fullSet) == 'X1stFlrSF'] <- 'FirstFlrSF'
names(fullSet)[names(fullSet) == 'X2ndFlrSF'] <- 'SecondFlrSF'
names(fullSet)[names(fullSet) == 'X3SsnPorch'] <- 'ThreeSsnPorch'

# resolve NA levls and set ordinal variables
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
