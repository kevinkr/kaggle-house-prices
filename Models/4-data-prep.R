
# Data Modeling Preparation -----------------------------------------------

###################################
#   From num var selection examination
# fullSet <- subset(fullSet, select = -c(MasVnrArea, HalfBath, BedroomAbvGr, GarageYrBlt,
#                                        PoolArea, MiscVal, MoSold, YrSold, TimeSinceSold,
#                                        countFa, countPo))
# #   From cat var selection examination
# fullSet <- subset(fullSet, select = -c(LandContour, HouseStyle, ExterCond, Season,
#                                        MSZoning, Street, Alley, Utilities, LotConfig,
#                                        Neighborhood, Condition1, RoofMatl, Exterior1st,
#                                        Exterior2nd, Foundation, Heating, PoolQC, 
#                                        MiscFeature, SaleType, BuiltAndSold, OverallCondCut,
#                                        RemodelAndSold, NearWater, CrimeLevel))
# 
# fullSet <- subset(fullSet, select = -c(FullBath, TotRmsAbvGrd, 
#                                         NewHouseSubClass))
# 
# fullSet <- subset(fullSet, select = -c(LotFrontage, FirstFlrSF, GrLiveAreaByFirstFlrSF,
#                                        OverallCondByQual, OverallCondExp, countTA,
#                                        x602, x607, x612, x613, x632, x633,
#                                        x635, x648, x926, x939, x943, x959,
#                                        x960, x966, x968, x1455, x1683, x1686,
#                                        x1689, x2441, x2448, x2458, x2512,
#                                        x2529, x2537, x2812, x2814, x2830, x2849,
#                                        x2858))

# revised variable selection based on stepwise analysis 2-7-17
fullSet <- subset(fullSet, select = c(AgeOfHouse,Alley,BathTotals,BedroomAbvGr,BldgType,BsmtBaths,
                                      BuiltAndSold,CentralAir,Condition1,countEx,countGd,countPo,countTA,
                                      Electrical,ExterQual,Fireplaces,FirstFloorOnly,FirstFlrSF,Foundation,FullBath,
                                      Functional,GarageFinish,GarageType,GarageYrBlt,GrLivArea,GrLivAreaCut,
                                      GrLiveAreaByFirstFlrSF,GrLiveAreaLog,HalfBath,Heating,HeatingQC,HouseStyle,
                                      KitchenAbvGr,KitchenQual,LandContour,LotArea,LotFrontage,LotShape,MasVnrArea,
                                      meanOverallNbrhdC,meanOverallNbrhdQ,MoSold,MSSubClass,MSZoning,NearAirport,
                                      NearCampus,NearWater,newBsmtFinTypeSF,newBsmtQualSF,newGarageSizeQual,
                                      outdoorSpace,OverallCond,OverallCondCubed,OverallCondExp,OverallQual,
                                      OverallQualCubed,OverallQualExp,PavedDrive,PoolArea,PoolQC,RoofStyle,
                                      SaleCondition,SouthOf30,Street,TimeSinceSold,x105,x478,x5,x538,x674,x685,
                                      x71,x890,x92,x94,x97,x977,x99))

#fullSet <- subset(fullSet, select = -c(OverallQual,GrLivArea,GrLiveAreaCubed,OverallQualCut))

if (resultsTable$l == 1) {
  ######################################
  # Feature l
  # Reduce multicollinearity
  # Multicollinearity of numeric variables ---------------------------------------------
  library(caret)
  correlCutOff <- 0.90
  num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
  df <- subset(fullSet, select = num.var)
  df2 = fullSet[,!(names(fullSet) %in% num.var)]
  descrCorr <- cor(df)
  descrCorr <- cor(fullSet[,(names(fullSet) %in% num.var)])
  highCorr <- findCorrelation(descrCorr, correlCutOff)
  colnames(df[,highCorr])
  # remove highly correlated  continuous variables
  if (length(highCorr) > 0) {
    df <- df[, -highCorr]
    fullSet <- cbind(df2,df)
  }
}

if (resultsTable$m == 1) {
  ################################################
  # Feature m
  # OHE
  library(dplyr)
  cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
  fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)
  
  # OHE 
  # use caret dummyVars function for hot one encoding for categorical features
  library(dummies)
  fullSet <- dummy.data.frame(fullSet, names=cat.var, sep="_")
  colSums(sapply(fullSet, is.na)) > 0 # list of vars with missing values
}

if (resultsTable$h == 1) {
  # Feature h
  # Scale of continuous values
  num.var <- sapply(fullSet, is.numeric)
  fullSet[num.var] <- data.frame(lapply(fullSet[num.var], scale))
}


if (resultsTable$i == 1) {
  ###################################
  # Feature i
  # Skewness of continuous values
  num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
  # determine skew for each numeric feature
  library(moments)
  skewed_feats <- sapply(num.var,function(x){skewness(fullSet[[x]],na.rm=TRUE)})
  
  # keep only features that exceed a threshold for skewness
  skewed_feats <- skewed_feats[abs(skewed_feats) > 0.80]
  skewed_feats
  # transform excessively skewed features with log(x + 1)
  for(x in names(skewed_feats)) {
    fullSet[[x]] <- log(fullSet[[x]] + 200)
  }
}

if (resultsTable$j == 1) {
  #####################################
  # Feature j
  # Reduce category variation 
  cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
  # Remove insignificant categorical variables --------------------
  ######
  # category reduction fucntion
  ######
  # inputs category name, cutoff value
  reduce_cats <- function(cat.name, cutoff.val) {
    prop.table <- sort(prop.table(table(fullSet[[cat.name]])), decreasing = T)
    #return(proptable)
    weak.prop.table <- prop.table < cutoff.val
    #return(weak.prop.table)
    # grab the names
    weak.prop.names <- names(prop.table[prop.table < cutoff.val])
    return(weak.prop.names)
  }
  
  ############full loop attempt
  for (n in cat.var) {
    #print(n)
    # call function to return category names for reduction, number is cutoff val
    #weak.prop.names <- reduce_cats(cat.name, 0.01)
    weak.prop.names <- reduce_cats(n, 0.01)
    # filter data set by categories that are in the weak prop names vector using %in% search'
    # first convert to character
    fullSet[[n]] <- as.character(fullSet[[n]])
    fullSet[fullSet[[n]] %in% weak.prop.names, n] <- "OTHER"
    fullSet[[n]] <- as.factor(fullSet[[n]])
  }
}


if (resultsTable$k == 1) {
  #################################
  # Feature k
  # Reduce zero, near zero variance
  library(caret)
  nzv_cols <- nearZeroVar(fullSet)
  names(fullSet[nzv_cols])
  if(length(nzv_cols) > 0)
    fullSet <- fullSet[, -nzv_cols]
}

##########################################
# split back into test and train
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)
# create data for training and test
X_train <- fullSet[1:nrow(train),]
X_test <- fullSet[(nrow(train)+1):nrow(fullSet),]


#y <- train$SalePrice
#test$SalePrice <- -99
rm(train.raw, test.raw)
gc()
# 
# features = names(fullSet)
# 
# for (f in features) {
#   if (class(fullSet[[f]])=="factor") {
#     #cat("VARIABLE : ",f,"\n")
#     levels <- unique(fullSet[[f]])
#     fullSet[[f]] <- as.integer(factor(fullSet[[f]], levels=levels))
#   }
# }
