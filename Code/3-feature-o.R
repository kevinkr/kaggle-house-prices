###########################################
# Kaggle Housing Prices
# Feature Engineering - 
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
