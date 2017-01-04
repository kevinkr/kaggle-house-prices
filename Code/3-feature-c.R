###########################################
# Kaggle Housing Prices
# Feature Engineering - 
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

