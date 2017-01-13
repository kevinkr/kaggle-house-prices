###########################################
# Kaggle Housing Prices
# Feature Engineering - 
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
fullSet$OverallQualCut <- 'Poor' 
fullSet$OverallQualCut[fullSet$OverallQual > 6] <- 'Good'
fullSet$OverallQualCut[fullSet$OverallQual > 4 & fullSet$OverallQual <= 6] <- 'Average' 
#table(fullSet$OverallQualCut)
fullSet$OverallQualCut <- ordered(fullSet$OverallQualCut, levels = c("Poor","Average","Good"))

# OverallCondCut
fullSet$OverallCondCut <- 'Poor' 
fullSet$OverallCondCut[fullSet$OverallCond > 6] <- 'Good'
fullSet$OverallCondCut[fullSet$OverallCond > 4 & fullSet$OverallQual <= 6] <- 'Average' 
#table(fullSet$OverallCondCut)
fullSet$OverallCondCut <- ordered(fullSet$OverallCondCut, levels = c("Poor","Average","Good"))
