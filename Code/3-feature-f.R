###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature f
# GrLiveArea / FirstFlrSF
fullSet$GrLiveAreaByFirstFlrSF <- fullSet$GrLivArea / fullSet$FirstFlrSF
fullSet$GrLiveAreaSq <- fullSet$GrLivArea * fullSet$GrLivArea
fullSet$GrLiveAreaCubed <- fullSet$GrLivArea * fullSet$GrLivArea * fullSet$GrLivArea
fullSet$GrLiveAreaExp <- exp(fullSet$GrLivArea)
fullSet$GrLiveAreaLog <- log(fullSet$GrLivArea)


# Baths related
fullSet$BathTotals <- fullSet$FullBath + fullSet$HalfBath
fullSet$BsmtBaths <- fullSet$BsmtFUllBath + fullSet$BsmtHalfBath

# OverallQual number gymnastics
fullSet$OverallQualSq <- fullSet$OverallQual * fullSet$OverallQual
fullSet$OverallQualCubed <- fullSet$OverallQual * fullSet$OverallQual * fullSet$OverallQual
fullSet$OverallQualExp <- exp(fullSet$OverallQual)

# OverallCond number gymnastics 
fullSet$OverallCondSq <- fullSet$OverallCond * fullSet$OverallCond
fullSet$OverallCondCubed <- fullSet$OverallCond * fullSet$OverallCond * fullSet$OverallCond
fullSet$OverallCondExp <- exp(fullSet$OverallCond)

