###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature d
# Create variable for first floor only
fullSet$FirstFloorOnly <- 'Yes'
fullSet$FirstFloorOnly[fullSet$SecondFlrSF > 0] <- 'No'
fullSet$FirstFloorOnly <- as.factor(fullSet$FirstFloorOnly)
fullSet <- subset(fullSet, select = -c(YearBuilt, YearRemodAdd,SecondFlrSF))