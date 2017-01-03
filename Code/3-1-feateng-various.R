# Kaggle Housing Prices
# Feature engineering
#
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



# Create variable for identifying new remodels (10 yrs. or less)
fullSet$RecentRemodel <- 0
fullSet$RecentRemodel[fullSet$YearRemodAdd - fullSet$YearBuilt <= 10] <- 1 
fullSet$RecentRemodel <- as.factor(fullSet$RecentRemodel)


# Create variable for first floor only
fullSet$FirstFloorOnly <- 'Yes'
fullSet$FirstFloorOnly[fullSet$SecondFlrSF > 0] <- 'No'
fullSet$FirstFloorOnly <- as.factor(fullSet$FirstFloorOnly)
fullSet <- subset(fullSet, select = -c(YearBuilt, YearRemodAdd,SecondFlrSF))

# Cut GrLiveArea
#hist(fullSet$GrLivArea, breaks=30)
fullSet$GrLivAreaCut<-as.factor(cut(fullSet$GrLivArea, c(0,1000,1200,1400,1600,1800,
                                               2000,2500,3000,6000), right=FALSE, 
                                               labels=c(1:9)))

# GrLiveArea / FirstFlrSF
fullSet$GrLiveAreaByFirstFlrSF <- fullSet$GrLivAreafullSet$FirstFlrSF

# GRLiveArea - zScore
#fullSet$GrLivAreaZscore <- scale(fullSet$GrLivArea, center = TRUE, scale = TRUE)[,]
fullSet <- subset(fullSet, select = -c(GrLivArea))
