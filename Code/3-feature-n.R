###########################################
# Kaggle Housing Prices
# Feature Engineering - 
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