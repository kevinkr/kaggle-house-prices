# starting with 83 variables
cat.var <- names(X_train)[which(sapply(X_train, is.factor))]
XX_train <- X_train
XX_train = XX_train[,(names(XX_train) %in% cat.var)]
# orig 0.1465099
XX_train <- subset(XX_train, select = -c(LandContour,HouseStyle,ExterCond,Season))
# after drop 0.1462604
# Drop MSSubClass, 0.1465009 KEEP
XX_train <- subset(XX_train, select = -c(MSSubClass))
X_train <- XX_train
# Drop MSZoning 0.1462732 DROP
XX_train <- subset(XX_train, select = -c(MSZoning))
X_train <- XX_train
# Drop Street 0.1460013 DROP
XX_train <- subset(XX_train, select = -c(Street))
X_train <- XX_train
# Drop Alley 0.145845 DROP
XX_train <- subset(XX_train, select = -c(Alley))
X_train <- XX_train
# Drop LotShape 0.1460359 KEEP
XX_train <- subset(XX_train, select = -c(LotShape))
X_train <- XX_train
# Drop Utilities 0.1460224 DROP
XX_train <- subset(XX_train, select = -c(Utilities))
X_train <- XX_train
# Drop LotConfig 0.1459936 DROP
XX_train <- subset(XX_train, select = -c(LotConfig))
X_train <- XX_train
# Drop LandSlope 0.1463825 KEEP
XX_train <- subset(XX_train, select = -c(LandSlope))
X_train <- XX_train
# Drop Neighborhood 0.146314 DROP
XX_train <- subset(XX_train, select = -c(Neighborhood))
X_train <- XX_train
# Drop Condition1 0.146239 DROP
XX_train <- subset(XX_train, select = -c(Condition1))
X_train <- XX_train
# Drop BldgType 0.1481069 KEEP
XX_train <- subset(XX_train, select = -c(BldgType))
X_train <- XX_train
# Drop OverallQual 0.1546519 KEEP
XX_train <- subset(XX_train, select = -c(OverallQual))
X_train <- XX_train
# Drop OverallCond 0.1570579 KEEP
XX_train <- subset(XX_train, select = -c(OverallCond))
X_train <- XX_train
# Drop RoofStyle 0.157463 KEEP
XX_train <- subset(XX_train, select = -c(RoofStyle))
X_train <- XX_train
# Drop RoofMatl 0.1574619 DROP
XX_train <- subset(XX_train, select = -c(RoofMatl))
X_train <- XX_train
# Drop Exterior1st 0.1574248 DROP
XX_train <- subset(XX_train, select = -c(Exterior1st))
X_train <- XX_train
# Drop Exterior2nd 0.1573465 DROP
XX_train <- subset(XX_train, select = -c(Exterior2nd))
X_train <- XX_train
# Drop MasVnrType 0.1575533 KEEP
XX_train <- subset(XX_train, select = -c(MasVnrType))
X_train <- XX_train
# Drop ExterQual 0.1593476 KEEP
XX_train <- subset(XX_train, select = -c(ExterQual))
X_train <- XX_train
# Drop Foundation 0.1592594 DROP
XX_train <- subset(XX_train, select = -c(Foundation))
X_train <- XX_train
# Drop Heating 0.1590982 DROP
XX_train <- subset(XX_train, select = -c(Heating))
X_train <- XX_train
# Drop HeatingQC 0.1596426 KEEP
XX_train <- subset(XX_train, select = -c(HeatingQC))
X_train <- XX_train
# Drop CentralAir 0.1629584 KEEP
XX_train <- subset(XX_train, select = -c(CentralAir))
X_train <- XX_train
# Drop Electrical 0.1634504 KEEP
XX_train <- subset(XX_train, select = -c(Electrical))
X_train <- XX_train
# Drop KitchenQual 0.1723589 KEEP
XX_train <- subset(XX_train, select = -c(KitchenQual))
X_train <- XX_train
# Drop Functional 0.1751189 KEEP
XX_train <- subset(XX_train, select = -c(Functional))
X_train <- XX_train
# Drop FireplaceQu 0.1771678 KEEP
XX_train <- subset(XX_train, select = -c(FireplaceQu))
X_train <- XX_train
# Drop GarageType 0.1772711 KEEP
XX_train <- subset(XX_train, select = -c(GarageType))
X_train <- XX_train
# Drop GarageFinish 0.1822396 KEEP
XX_train <- subset(XX_train, select = -c(GarageFinish))
X_train <- XX_train
# Drop PavedDrive 0.1851058 KEEP
XX_train <- subset(XX_train, select = -c(PavedDrive))
X_train <- XX_train
# Drop PoolQC 0.1849478 DROP
XX_train <- subset(XX_train, select = -c(PoolQC))
X_train <- XX_train
# Drop Fence 0.1848459 KEEP
XX_train <- subset(XX_train, select = -c(Fence))
X_train <- XX_train
# Drop MiscFeature  0.1847366 DROP
XX_train <- subset(XX_train, select = -c(MiscFeature))
X_train <- XX_train
# Drop SaleType 0.1846303 DROP
XX_train <- subset(XX_train, select = -c(SaleType))
X_train <- XX_train
# Drop SaleCondition 0.1874023 KEEP
XX_train <- subset(XX_train, select = -c(SaleCondition))
X_train <- XX_train
# Drop DecadeBuilt 0.1885038 KEEP
XX_train <- subset(XX_train, select = -c(DecadeBuilt))
X_train <- XX_train
# Drop BuiltAndSold 0.1883969 DROP
XX_train <- subset(XX_train, select = -c(BuiltAndSold))
X_train <- XX_train
# Drop SellingSeason 0.1885795 KEEP
XX_train <- subset(XX_train, select = -c(SellingSeason))
X_train <- XX_train
# Drop NewHouseSubClass 0.1916535 KEEP
XX_train <- subset(XX_train, select = -c(NewHouseSubClass))
X_train <- XX_train
# Drop OverallQualCut 0.2218414 KEEP
XX_train <- subset(XX_train, select = -c(OverallQualCut))
X_train <- XX_train
# Drop OverallCondCut 0.2217015 DROP
XX_train <- subset(XX_train, select = -c(OverallCondCut))
X_train <- XX_train
# Drop RecentRemodel 0.2241446 KEEP
XX_train <- subset(XX_train, select = -c(RecentRemodel))
X_train <- XX_train
# Drop HasBeenRemodeled 0.2242252 KEEP
XX_train <- subset(XX_train, select = -c(HasBeenRemodeled))
X_train <- XX_train
# Drop RemodelAndSold 0.2242252 DROP
XX_train <- subset(XX_train, select = -c(RemodelAndSold))
X_train <- XX_train
# Drop FirstFloorOnly 0.2475485 KEEP
XX_train <- subset(XX_train, select = -c(FirstFloorOnly))
X_train <- XX_train
# Drop GrLivAreaCut 0.3427375 KEEP
XX_train <- subset(XX_train, select = -c(GrLivAreaCut))
X_train <- XX_train
# Drop NearCampus 0.3514083 KEEP
XX_train <- subset(XX_train, select = -c(NearCampus))
X_train <- XX_train
# Drop SouthOf30 0.3533459 KEEP
XX_train <- subset(XX_train, select = -c(SouthOf30))
X_train <- XX_train
# Drop NearAirport 0.3597805 KEEP
XX_train <- subset(XX_train, select = -c(NearAirport))
X_train <- XX_train
# Drop NearWater 0.3597805 DROP
XX_train <- subset(XX_train, select = -c(NearWater))
X_train <- XX_train
# Drop CrimeLevel 0.3597805 DROP
XX_train <- subset(XX_train, select = -c(CrimeLevel))
X_train <- XX_train

X_train <- data.matrix(XX_train)
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)
# test out Ridge regression model
lambdas <- seq(1,0,-0.001)
# train model
set.seed(123)  # for reproducibility
model_ridge <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))
ggplot(data=filter(model_ridge$result,RMSE<0.15)) +
  geom_line(aes(x=lambda,y=RMSE))
mean(model_ridge$resample$RMSE)
# test out Lasso regression model
# train model
set.seed(123)  # for reproducibility
model_lasso <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
mean(model_lasso$resample$RMSE)




# extract coefficients for the best performing model
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

# print summary of model results
picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")

# sort coefficients in ascending order
coef <- arrange(coef,-coef.value)

