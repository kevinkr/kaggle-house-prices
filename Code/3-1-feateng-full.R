# Kaggle House prices
#
# Feature engineering and results tracking test code

# Define list of features
# i.e. A, B, C, D
# these are features in addition to core code
# A - Basement variables
# B - Time-based changed...Decade built, new house, seasons, selling season
# C - Remodelling related
# D - First floor only indicator
# E - Area-based engineering - Cuts of GrLivArea, sum of outdoor space
# F - GrLivArea / FirstFlrSF
# G - Reduce variables on certain categories with low representation
# H - Scale all numeric variables
# I - Adjust skewness of all numerica variable
# J - Reduce category variation
# K - Reduce zero, near zero variance
# L - Reduce multicollinarity of numeric variables
# M - OHE
# N - Geographical Features
# O - Crime Level
# P - Count rowsums on guality parameters
# load from previous steps
source("Code/1-load-data.R")
source("Code/2-1-eda.R")
feature_list <- list(a=1, b=1, c=1, d=1, e=1, f=1, g=1, h=1, i=1, j=1, k=0, l=0, m=0, n=1, o=1, p=1)
# Create df with feature settings
resultsTable <- data.frame(feature_list)


if (resultsTable$f == 1) {
  ##############################
  # Feature f
  # GrLiveArea / FirstFlrSF
  fullSet$GrLiveAreaByFirstFlrSF <- fullSet$GrLivArea / fullSet$FirstFlrSF
  
  fullSet$OverallCondByQual <- as.numeric(fullSet$OverallCond)/as.numeric(fullSet$OverallQual)
  
  # DROP fullSet$GrLiveAreaSq <- fullSet$GrLivArea * fullSet$GrLivArea
  fullSet$GrLiveAreaCubed <- fullSet$GrLivArea * fullSet$GrLivArea * fullSet$GrLivArea
  #fullSet$GrLiveAreaExp <- exp(as.numeric(fullSet$GrLivArea))
  fullSet$GrLiveAreaLog <- log(as.numeric(fullSet$GrLivArea))
  
  
  # Baths related
  fullSet$BathTotals <- fullSet$FullBath + fullSet$HalfBath
  fullSet$BsmtBaths <- fullSet$BsmtFullBath + fullSet$BsmtHalfBath
  
  # OverallQual number gymnastics
  # DROP fullSet$OverallQualSq <- as.numeric(fullSet$OverallQual) * as.numeric(fullSet$OverallQual)
  fullSet$OverallQualCubed <- as.numeric(fullSet$OverallQual) * as.numeric(fullSet$OverallQual) * as.numeric(fullSet$OverallQual)
  fullSet$OverallQualExp <- exp(as.numeric(fullSet$OverallQual))
  
  # OverallCond number gymnastics 
  fullSet$OverallCondSq <- as.numeric(fullSet$OverallCond) * as.numeric(fullSet$OverallCond)
  fullSet$OverallCondCubed <- as.numeric(fullSet$OverallCond) * fullSet$OverallCondSq
  fullSet$OverallCondExp <- exp(as.numeric(fullSet$OverallCond))
}

if (resultsTable$a == 1) {
  ###########################
  # Feature Engineering 
  #
  # Feature a
  # 
  # Create new features combining basement & garage information
  fullSet$newBsmtQualSF <- as.numeric(factor(fullSet$BsmtQual, levels=c("No Bsmnt", "Po",
                                                                        "Fa", "TA", "Gd", "Ex")))
  fullSet$newBsmtQualSF <- fullSet$newBsmtQualSF * fullSet$TotalBsmtSF
  
  # Bsmt Fin Type 1
  fullSet$newBsmtFinTypeSF <- as.numeric(factor(fullSet$BsmtFinType1, levels=c("No Bsmnt", "Unf",
                                                                               "LWQ", "Rec", "BLQ", "ALQ", "GLQ")))
  fullSet$newBsmtFinTypeSF <- fullSet$newBsmtFinTypeSF * fullSet$BsmtFinSF1
  
  # Garage new feature
  fullSet$newGarageSizeQual <- as.numeric(factor(fullSet$GarageQual, levels=c("No Garage", "Po",
                                                                              "Fa", "TA", "Gd", "Ex")))
  fullSet$newGarageSizeQual <- fullSet$newGarageSizeQual * 
                                        as.numeric(factor(fullSet$GarageCond, levels=c("No Garage", "Po",
                                                                                       "Fa", "TA", "Gd", "Ex")))
  fullSet$newGarageSizeQual <- fullSet$newGarageSizeQual * fullSet$GarageArea
  
  fullSet <- subset(fullSet, select = -c(BsmtHalfBath,BsmtFinSF2,BsmtFinType2,BsmtFullBath, 
                                         BsmtCond, BsmtUnfSF, BsmtExposure))
  fullSet <- subset(fullSet, select = -c(BsmtQual,TotalBsmtSF,BsmtFinType1,BsmtFinSF1,LowQualFinSF))
  fullSet <- subset(fullSet, select = -c(GarageArea,GarageQual,GarageCond,GarageCars))
}


if (resultsTable$b == 1) {
  ######################################
  # Feature b
  # Time-based factors
  # Create variable splitting Year built into decades
  #hist(fullSet$YearBuilt, breaks=20)
  fullSet$DecadeBuilt <- 'BuiltBefore1920'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 1940 & fullSet$YearBuilt >= 1920] <- '1920-1940'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 1960 & fullSet$YearBuilt >= 1940] <- '1940-1960'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 1970 & fullSet$YearBuilt >= 1960] <- '1960-1970'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 1980 & fullSet$YearBuilt >= 1970] <- '1970-1980'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 1990 & fullSet$YearBuilt >= 1980] <- '1980-1990'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 2000 & fullSet$YearBuilt >= 1990] <- '1990-2000'
  fullSet$DecadeBuilt[fullSet$YearBuilt < 2010 & fullSet$YearBuilt >= 2000] <- '2000-2010'
  fullSet$DecadeBuilt[fullSet$YearBuilt >= 2010] <- '2010-'
  fullSet$DecadeBuilt <- as.factor(fullSet$DecadeBuilt)
  
  
  
  # House sold in year it was built
  fullSet$BuiltAndSold <- 0
  fullSet$BuiltAndSold[fullSet$YearBuilt == fullSet$YrSold] <- 1 
  fullSet$BuiltAndSold <- as.factor(fullSet$BuiltAndSold)
  
  
  # Examine month sold
  hist(fullSet$MoSold, breaks=20)
  fullSet$SellingSeason <- 0
  fullSet$SellingSeason[fullSet$MoSold >= 4 & fullSet$MoSold <= 7] <- 1
  fullSet$SellingSeason <- as.factor(fullSet$SellingSeason)
  
  # Create Season category
  fullSet$Season[fullSet$MoSold == 12 | fullSet$MoSold == 1 | fullSet$MoSold == 2] <- 'Winter'
  fullSet$Season[fullSet$MoSold == 3 | fullSet$MoSold == 4 | fullSet$MoSold == 5] <- 'Spring'
  fullSet$Season[fullSet$MoSold == 6 | fullSet$MoSold == 7 | fullSet$MoSold == 8] <- 'Summer'
  fullSet$Season[fullSet$MoSold == 9 | fullSet$MoSold == 10 | fullSet$MoSold == 11] <- 'Fall'
  fullSet$Season <- as.factor(fullSet$Season)
  
  
  # New house variable based on MSSubClass 
  fullSet$NewHouseSubClass <- 0
  fullSet$NewHouseSubClass[fullSet$MSSubClass == 20 | fullSet$MSSubClass == 60 |
                             fullSet$MSSubClass == 120 | fullSet$MSSubClass == 160] <- 1
  fullSet$NewHouseSubClass <- as.factor(fullSet$NewHouseSubClass)
  
  
  # Age of house
  fullSet$AgeOfHouse <- 2010 - fullSet$YearBuilt
  
  # Time since house sold
  fullSet$TimeSinceSold <- 2010 - fullSet$YrSold
  
  # OverallQualityCut
  fullSet$OverallQualCut <- 'Po' 
  fullSet$OverallQualCut[fullSet$OverallQual > 6] <- 'Gd'
  fullSet$OverallQualCut[fullSet$OverallQual > 4 & fullSet$OverallQual <= 6] <- 'TA' 
  #table(fullSet$OverallQualCut)
  fullSet$OverallQualCut <- ordered(fullSet$OverallQualCut, levels = c("Po","TA","Gd"))
  
  # OverallCondCut
  fullSet$OverallCondCut <- 'Po' 
  fullSet$OverallCondCut[fullSet$OverallCond > 6] <- 'Gd'
  fullSet$OverallCondCut[fullSet$OverallCond > 4 & fullSet$OverallQual <= 6] <- 'TA' 
  #table(fullSet$OverallCondCut)
  fullSet$OverallCondCut <- ordered(fullSet$OverallCondCut, levels = c("Po","TA","Gd"))
}

if (resultsTable$c == 1) {
  ############################
  # Feature c
  # Create variable for identifying new remodels (10 yrs. or less)
  fullSet$RecentRemodel <- 0
  fullSet$RecentRemodel[fullSet$YearRemodAdd - fullSet$YearBuilt <= 10] <- 1 
  fullSet$RecentRemodel <- as.factor(fullSet$RecentRemodel)
  
  # create variable to indicate remodelling occurred
  fullSet$HasBeenRemodeled <- 0
  fullSet$HasBeenRemodeled[fullSet$YearRemodAdd != fullSet$YearBuilt] <- 1 
  fullSet$HasBeenRemodeled <- as.factor(fullSet$HasBeenRemodeled)
  
  # Variable to indicate if remodeling occurred in year sold
  fullSet$RemodelAndSold <- 0
  fullSet$RemodelAndSold[fullSet$YearRemodAdd == fullSet$YrSold] <- 1 
  fullSet$RemodelAndSold <- as.factor(fullSet$RemodelAndSold)
}

if (resultsTable$d == 1) {
  ##############################
  # Feature d
  # Create variable for first floor only
  fullSet$FirstFloorOnly <- 'Yes'
  fullSet$FirstFloorOnly[fullSet$SecondFlrSF > 0] <- 'No'
  fullSet$FirstFloorOnly <- as.factor(fullSet$FirstFloorOnly)
  fullSet <- subset(fullSet, select = -c(YearBuilt, YearRemodAdd,SecondFlrSF))
}


if (resultsTable$e == 1) {
  ##############################
  # Feature e
  # Area based engineering
  
  # Cut GrLiveArea
  #hist(fullSet$GrLivArea, breaks=30)
  fullSet$GrLivAreaCut<-as.factor(cut(fullSet$GrLivArea, c(0,1000,1200,1400,1600,1800,
                                                           2000,2500,3000,6000), right=FALSE, 
                                      labels=c(1:9)))
  
  # sum outdoor space
  fullSet$outdoorSpace <- fullSet$OpenPorchSF + fullSet$EnclosedPorch + fullSet$ThreeSsnPorch +
    fullSet$ScreenPorch + fullSet$OpenPorchSF + fullSet$WoodDeckSF
  fullSet <- subset(fullSet, select = -c(OpenPorchSF, EnclosedPorch, ThreeSsnPorch, 
                                         ScreenPorch, OpenPorchSF, WoodDeckSF))
}


if (resultsTable$g == 1) {
  ##############################
  # Feature G
  # Simplify various categories
  #
  ## Lot Shape
  # table(fullSet$LotShape)
  # IR2 and Ir3 don't appear too often, combine into regular and irregular
  levels(fullSet$LotShape)[levels(fullSet$LotShape)!="Reg"] <- "Irregular"
  
  ## LandContour
  # table(fullSet$LandContour)
  # Most if level, simplify to Lvl and NotLvl
  levels(fullSet$LandContour)[levels(fullSet$LandContour)!="Lvl"] <- "NotLvl"
  
  ## LandSlope
  # table(fullSet$LandSlope)
  # Most are gentle, change others to NotGentle
  levels(fullSet$LandSlope)[levels(fullSet$LandSlope)!="Gtl"] <- "NotGentle"
  
  ## Electrical
  # table(fullSet$Electrical)
  # Most use Standard Breaker
  levels(fullSet$Electrical)[levels(fullSet$Electrical)!="SBrkr"] <- "NotSBrkr"
  
  ## GarageType
  #table(fullSet$GarageType)
  # Most have Attached or Detached or No Garage, combine others into Other
  levels(fullSet$GarageType)[levels(fullSet$GarageType)=="2Types"] <- "OtherGarage"
  levels(fullSet$GarageType)[levels(fullSet$GarageType)=="Basment"] <- "OtherGarage"
  levels(fullSet$GarageType)[levels(fullSet$GarageType)=="CarPort"] <- "OtherGarage"
  
  ## PavedDrive
  # table(fullSet$PavedDrive)
  # Most are Yes, all others called Unpaved
  levels(fullSet$PavedDrive)[levels(fullSet$PavedDrive)!="Y"] <- "Unpaved"
  
  ## MiscFeature
  # table(fullSet$MiscFeature)
  # Group all non-shed entries into Other
  levels(fullSet$MiscFeature)[levels(fullSet$MiscFeature)=="Shed"] <- "Other"
  
  ## MSZoning 
  # table(fullSet$MSZoning)
  # Group all non-residential entries into Other
  levels(fullSet$MSZoning)[levels(fullSet$MSZoning)=="C (All)"] <- "Other"
  levels(fullSet$MSZoning)[levels(fullSet$MSZoning)=="RH"] <- "Other"
  
  # Condition 1 and 2 
  levels(fullSet$Condition1) <- gsub("^RR..$", "Noisy", levels(fullSet$Condition1))
  levels(fullSet$Condition1) <- gsub("^Pos.$", "By Park", levels(fullSet$Condition1))
  levels(fullSet$Condition1)[levels(fullSet$Condition1)=="Artery" | levels(fullSet$Condition1)=="Feedr"] <- "Busy Road"
  # drop Condition2, no extra relevant information
  fullSet <- subset(fullSet, select = -c(Condition2))
}

if (resultsTable$n == 1) {
  ###################################
  # Feature n
  # Geographcal features
  # 
  # Near Campus
  fullSet$NearCampus <- 0
  fullSet$NearCampus[fullSet$Neighborhood == 'BrkSide' | fullSet$Neighborhood == 'ClearCr' |
                       fullSet$Neighborhood == 'Crawfor' | fullSet$Neighborhood == 'IDOTRR' |
                       fullSet$Neighborhood == 'SWISU' ] <- 1 
  fullSet$NearCampus <- as.factor(fullSet$NearCampus)
  table(fullSet$NearCampus)
  
  # South of US 30
  fullSet$SouthOf30 <- 0
  fullSet$SouthOf30[fullSet$Neighborhood == 'MeadowV' | fullSet$Neighborhood == 'Mitchel' |
                      fullSet$Neighborhood == 'Timber'] <- 1 
  fullSet$SouthOf30 <- as.factor(fullSet$SouthOf30)
  table(fullSet$SouthOf30)
  
  # Near Airport
  fullSet$NearAirport <- 0
  fullSet$NearAirport[fullSet$Neighborhood == 'Blueste' | fullSet$Neighborhood == 'Crawfor' |
                        fullSet$Neighborhood == 'Timber'] <- 1 
  fullSet$NearAirport <- as.factor(fullSet$NearAirport)
  table(fullSet$NearAirport)
  
  # Near Water
  fullSet$NearWater <- 0
  fullSet$NearWater[fullSet$Neighborhood == 'ClearCr' | fullSet$Neighborhood == 'CollgCr' |
                      fullSet$Neighborhood == 'Gilbert' | fullSet$Neighborhood == 'IDOTRR' |
                      fullSet$Neighborhood == 'Names' | fullSet$Neighborhood == 'NoRidge' |
                      fullSet$Neighborhood == 'StoneBr' ] <- 1 
  fullSet$NearWater <- as.factor(fullSet$NearWater)
  table(fullSet$NearWater)
  
  # Next to Univ Blvd
  fullSet$NearWater <- 0
  fullSet$NearWater[fullSet$Neighborhood == 'ClearCr' | fullSet$Neighborhood == 'CollgCr' |
                      fullSet$Neighborhood == 'Gilbert' | fullSet$Neighborhood == 'IDOTRR' |
                      fullSet$Neighborhood == 'Names' | fullSet$Neighborhood == 'NoRidge' |
                      fullSet$Neighborhood == 'StoneBr' ] <- 1 
  fullSet$NearWater <- as.factor(fullSet$NearWater)
  table(fullSet$NearWater)
  
  # add mean and list of neighbors
  #fullSet <- merge(fullSet, neighborhoodNeighbors, by = "Neighborhood") 
  #library(plyr)
  # add mean of overall quality
  meanOverallQual <- ddply(fullSet, .(Neighborhood), plyr::summarize,  meanOverallNbrhdQ=mean(as.numeric(OverallQual)))
  fullSet <- join(fullSet, meanOverallQual, by = "Neighborhood") 
  
  # add mean of overall condition
  meanOverallCond <- ddply(fullSet, .(Neighborhood), plyr::summarize,  meanOverallNbrhdC=mean(as.numeric(OverallCond)))
  fullSet <- join(fullSet, meanOverallCond, by = "Neighborhood") 
}


if (resultsTable$o == 1) {
  ########################################
  # Feature o
  # Crime level
  # 
  fullSet$CrimeLevel <- 2
  fullSet$CrimeLevel[fullSet$Neighborhood == 'NoRidge'] <- 1
  fullSet$CrimeLevel[fullSet$Neighborhood == 'Blueste' | fullSet$Neighborhood == 'ClearCr' |
                       fullSet$Neighborhood == 'Crawfor' | fullSet$Neighborhood == 'Names' |
                       fullSet$Neighborhood == 'NWAmes' ] <- 3 
  fullSet$CrimeLevel[fullSet$Neighborhood == 'BrkSide' | fullSet$Neighborhood == 'Edwards' |
                       fullSet$Neighborhood == 'IDOTRR' | fullSet$Neighborhood == 'SWISU'  ] <- 4
  fullSet$CrimeLevel[fullSet$Neighborhood == 'CollgCr' | fullSet$Neighborhood == 'OldTown' ] <- 5
  fullSet$CrimeLevel <- as.factor(fullSet$CrimeLevel)
  fullSet$CrimeLevel <- ordered(fullSet$CrimeLevel, levels = c(1,2,3,4,5))
  #table(fullSet$CrimeLevel)
}


if (resultsTable$p == 1) {
  ########################################
  # Feature p
  # Row sums of Quality counts
  # 
  fullSet$countEx <- rowSums(fullSet == "Ex")
  fullSet$countGd <- rowSums(fullSet == "Gd")
  fullSet$countTA <- rowSums(fullSet == "TA")
  fullSet$countFa <- rowSums(fullSet == "Fa")
  fullSet$countPo <- rowSums(fullSet == "Po")
}



