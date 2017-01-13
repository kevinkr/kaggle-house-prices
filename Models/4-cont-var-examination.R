# starting with 83 variables
cat.var <- names(X_train)[which(sapply(X_train, is.factor))]
# 53
num.var <- names(X_train)[which(sapply(X_train, is.numeric))]
# 30

# 0.146, DONE - Drop all numeric vars
# X_train = X_train[,(names(X_train) %in% cat.var)]

# 0.131, DONE - Drop all categorical vars
# X_train = X_train[,(names(X_train) %in% num.var)]

# 0.1316, Focus on original on continuous vars
XX_train <- X_train
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]

XX_train = XX_train[,(names(XX_train) %in% num.var)]
X_train <- XX_train
# 0.1141017

# Drop GrLiveAreaSq 0.1141005 DROP
XX_train <- subset(XX_train, select = -c(GrLiveAreaSq))
X_train <- XX_train

# Drop GrLiveAreaCubed 0.1142731 KEEP
XX_train <- subset(XX_train, select = -c(GrLiveAreaCubed))
X_train <- XX_train

# Drop GrLiveAreaLog 0.1151402 KEEP
XX_train <- subset(XX_train, select = -c(GrLiveAreaLog))
X_train <- XX_train
# Drop BathTotals 0.1156325 KEEP
XX_train <- subset(XX_train, select = -c(BathTotals))
X_train <- XX_train
# Drop BsmtBaths  0.115804 KEEP
XX_train <- subset(XX_train, select = -c(BsmtBaths))
X_train <- XX_train
# Drop OverallQualSq  0.1157048 DROP
XX_train <- subset(XX_train, select = -c(OverallQualSq))
X_train <- XX_train
# Drop OverallQualCubed 0.1158811 KEEP
XX_train <- subset(XX_train, select = -c(OverallQualCubed))
X_train <- XX_train
# Drop OverallCondSq 0.1208925 KEEP
XX_train <- subset(XX_train, select = -c(OverallCondSq))
X_train <- XX_train
# Drop OverallCondExp  0.1220605 KEEP
XX_train <- subset(XX_train, select = -c(OverallCondExp))
X_train <- XX_train
# Drop x595 0.1222852 KEEP
XX_train <- subset(XX_train, select = -c(x595))
X_train <- XX_train
# Drop x596  0.1222563 DROP
XX_train <- subset(XX_train, select = -c(x596))
X_train <- XX_train
# Drop x597 0.1221924 DROP
XX_train <- subset(XX_train, select = -c(x597))
X_train <- XX_train
# Drop x598 0.122043 DROP
XX_train <- subset(XX_train, select = -c(x598))
X_train <- XX_train
# Drop x599 0.1219366 DROP
XX_train <- subset(XX_train, select = -c(x599))
X_train <- XX_train
# Drop x600 0.122185 DROP
XX_train <- subset(XX_train, select = -c(x600))
X_train <- XX_train
# Drop x601 0.1221519 DROP
XX_train <- subset(XX_train, select = -c(x601))
X_train <- XX_train
# Drop x602 0.1236596 KEEP
XX_train <- subset(XX_train, select = -c(x602))
X_train <- XX_train
# Drop x603 0.1235201 DROP
XX_train <- subset(XX_train, select = -c(x603))
X_train <- XX_train
# Drop x604 0.1235002 DROP
XX_train <- subset(XX_train, select = -c(x604))
X_train <- XX_train
# Drop x605 0.1234262 DROP
XX_train <- subset(XX_train, select = -c(x605))
X_train <- XX_train
# Drop x606 0.1234629 KEEP
XX_train <- subset(XX_train, select = -c(x606))
X_train <- XX_train
# Drop x607 0.1235035 KEEP
XX_train <- subset(XX_train, select = -c(x607))
X_train <- XX_train
# Drop x608 0.1242931 KEEP
XX_train <- subset(XX_train, select = -c(x608))
X_train <- XX_train
# Drop x609 0.1242044 DROP
XX_train <- subset(XX_train, select = -c(x609))
X_train <- XX_train
# Drop x610 0.1243501 KEEP
XX_train <- subset(XX_train, select = -c(x610))
X_train <- XX_train
# Drop x611 0.1246038 KEEP
XX_train <- subset(XX_train, select = -c(x611))
X_train <- XX_train
# Drop x612 0.124617 KEEP
XX_train <- subset(XX_train, select = -c(x612))
X_train <- XX_train
# Drop x613 0.1247984 KEEP
XX_train <- subset(XX_train, select = -c(x613))
X_train <- XX_train
####################
# 0.112382
# Drop x614 0.1123123 DROP
XX_train <- subset(XX_train, select = -c(x614))
X_train <- XX_train
# Drop x615 0.1123196 DROP
XX_train <- subset(XX_train, select = -c(x615))
X_train <- XX_train
# Drop x616 0.1125508 KEEP
XX_train <- subset(XX_train, select = -c(x616))
X_train <- XX_train
# Drop x617  0.1123842 DROP
XX_train <- subset(XX_train, select = -c(x617))
X_train <- XX_train
# Drop x618  0.1123502 DROP
XX_train <- subset(XX_train, select = -c(x618))
X_train <- XX_train
# Drop x619 0.112365 DROP
XX_train <- subset(XX_train, select = -c(x619))
X_train <- XX_train
# Drop x620 0.1123401 DORP
XX_train <- subset(XX_train, select = -c(x620))
X_train <- XX_train
# Drop x621 0.1123197 DROP
XX_train <- subset(XX_train, select = -c(x621))
X_train <- XX_train
# Drop x622 0.1119585 DROP
XX_train <- subset(XX_train, select = -c(x622))
X_train <- XX_train
# Drop x623 0.111718 DROP
XX_train <- subset(XX_train, select = -c(x623))
X_train <- XX_train
# Drop x624 0.1126563 KEEP
XX_train <- subset(XX_train, select = -c(x624))
X_train <- XX_train
# Drop x625 0.1126324 DROP
XX_train <- subset(XX_train, select = -c(x625))
X_train <- XX_train
# Drop x626 0.1125856 DROP
XX_train <- subset(XX_train, select = -c(x626))
X_train <- XX_train
# Drop x627 0.1125436 DROP
XX_train <- subset(XX_train, select = -c(x627))
X_train <- XX_train
# Drop x628 0.1125144 DROP
XX_train <- subset(XX_train, select = -c(x628))
X_train <- XX_train
# Drop x629 0.1125025 DROP
XX_train <- subset(XX_train, select = -c(x629))
X_train <- XX_train
# Drop x630 0.1124894 DROP
XX_train <- subset(XX_train, select = -c(x630))
X_train <- XX_train
# Drop x631 0.1124817 DROP
XX_train <- subset(XX_train, select = -c(x631))
X_train <- XX_train
# Drop x632 0.1127278 KEEP
XX_train <- subset(XX_train, select = -c(x632))
X_train <- XX_train
# Drop x633 0.1129562 KEEP
XX_train <- subset(XX_train, select = -c(x633))
X_train <- XX_train
# Drop x634 0.1129557 Drop
XX_train <- subset(XX_train, select = -c(x634))
X_train <- XX_train
# Drop x635 0.1132205 KEEP
XX_train <- subset(XX_train, select = -c(x635))
X_train <- XX_train
# Drop x636 0.1132707 KEEP
XX_train <- subset(XX_train, select = -c(x636))
X_train <- XX_train
# Drop x636 0.1132707 DROP
XX_train <- subset(XX_train, select = -c(x636))
X_train <- XX_train
# Drop x637 0.1132641 DROP
XX_train <- subset(XX_train, select = -c(x637))
X_train <- XX_train
# Drop x638 0.1132641 DROP
XX_train <- subset(XX_train, select = -c(x638))
X_train <- XX_train
# Drop x639 0.1132641 DROP
XX_train <- subset(XX_train, select = -c(x639))
X_train <- XX_train
# Drop x640 0.1132189 DROP
XX_train <- subset(XX_train, select = -c(x640))
X_train <- XX_train
# Drop x641 0.1131319 DROP
XX_train <- subset(XX_train, select = -c(x641))
X_train <- XX_train
# Drop x642 0.1131023 DROP
XX_train <- subset(XX_train, select = -c(x642))
X_train <- XX_train
# Drop x643 0.1129992 DROP
XX_train <- subset(XX_train, select = -c(x643))
X_train <- XX_train
# Drop x644 0.1129663 DROP
XX_train <- subset(XX_train, select = -c(x644))
X_train <- XX_train
# Drop x645 0.1130249 KEEP
XX_train <- subset(XX_train, select = -c(x645))
X_train <- XX_train
# Drop x646 0.112996 DROP
XX_train <- subset(XX_train, select = -c(x646))
X_train <- XX_train
# Drop x647 0.1131118 KEEP
XX_train <- subset(XX_train, select = -c(x647))
X_train <- XX_train
# Drop x648 0.1137929 KEEP
XX_train <- subset(XX_train, select = -c(x648))
X_train <- XX_train

X_train <- data.matrix(XX_train)
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)

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


# Drop LotArea, 0.133 KEEP
XX_train <- subset(XX_train, select = -c(LotArea))
X_train <- XX_train

# Drop MasVnrArea, 0.133 DROP
XX_train <- subset(XX_train, select = -c(MasVnrArea))
X_train <- XX_train

# Drop FirstFlrSF, 0.134 KEEP
XX_train <- subset(XX_train, select = -c(FirstFlrSF))
X_train <- XX_train

# Drop GrLivArea, 0.1421 KEEP
XX_train <- subset(XX_train, select = -c(GrLivArea))
X_train <- XX_train

# Drop FullBath, 0.1432 KEEP
XX_train <- subset(XX_train, select = -c(FullBath))
# Drop HalfBath, 0.1439 DROP
XX_train <- subset(XX_train, select = -c(HalfBath))
# Drop BedroomAbvGr, 0.144 DROP
XX_train <- subset(XX_train, select = -c(BedroomAbvGr))
# Drop KitchenAbvGr, 0.1457 KEEP
XX_train <- subset(XX_train, select = -c(KitchenAbvGr))
# Drop TotRmsAbvGrd, 0.1563 KEEP
XX_train <- subset(XX_train, select = -c(TotRmsAbvGrd))
# Drop Fireplaces, 0.1578 KEEP
XX_train <- subset(XX_train, select = -c(Fireplaces))
# Drop GarageYrBlt, 0.1579 DROP
XX_train <- subset(XX_train, select = -c(GarageYrBlt))
# Drop PoolArea, 0.1578 DROP
XX_train <- subset(XX_train, select = -c(PoolArea))
# Drop MiscVal, 0.1575 DROP
XX_train <- subset(XX_train, select = -c(MiscVal))
# Drop MoSold, 0.1573 DROP
XX_train <- subset(XX_train, select = -c(MoSold))
# Drop YrSold, 0.1573 DROP
XX_train <- subset(XX_train, select = -c(YrSold))
# Drop newBsmtQualSF, 0.1750 KEEP
XX_train <- subset(XX_train, select = -c(newBsmtQualSF))
# Drop newBsmtFinTypeSF, 0.1842 KEEP
XX_train <- subset(XX_train, select = -c(newBsmtFinTypeSF))
# Drop newGarageSizeQual, 0.1969 KEEP
XX_train <- subset(XX_train, select = -c(newGarageSizeQual))
# Drop AgeOfHouse, 0.20143 KEEP
XX_train <- subset(XX_train, select = -c(AgeOfHouse))
# Drop TimeSinceSold, 0.2013 DROP
XX_train <- subset(XX_train, select = -c(TimeSinceSold))
# Drop outdoorSpace, 0.20974 KEEP
XX_train <- subset(XX_train, select = -c(outdoorSpace))
# Drop GrLiveAreaByFirstFlrSF, 0.21000 KEEP
XX_train <- subset(XX_train, select = -c(GrLiveAreaByFirstFlrSF))
# Drop OverallCondByQual, 0.2157 KEEP
XX_train <- subset(XX_train, select = -c(OverallCondByQual))
# Drop meanOverallNbrhdQ, 0.2367 KEEP
XX_train <- subset(XX_train, select = -c(meanOverallNbrhdQ))
# Drop meanOverallNbrhdC, 0.2438 KEEP
XX_train <- subset(XX_train, select = -c(meanOverallNbrhdC))
# Drop countEx, 0.2969 KEEP
XX_train <- subset(XX_train, select = -c(countEx))
# Drop countGd, 0.3006 KEEP
XX_train <- subset(XX_train, select = -c(countGd))
# Drop countTA, 0.3694 KEEP
XX_train <- subset(XX_train, select = -c(countTA))
# Drop countFa, 0.3694 DROP
XX_train <- subset(XX_train, select = -c(countFa))
# Drop countPo, 0.3694 DROP
XX_train <- subset(XX_train, select = -c(countPo))


