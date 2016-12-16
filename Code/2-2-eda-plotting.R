
# EDA Plotting ------------------------------------------------------------
# load previous code
source("Code/2-1-eda.R")

###########################
## PLOTTING
########################
# continuous density plot
cont_nums <- c("LotFrontage","LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
               "TotalBsmtSF","1stFlrSF","2ndFlrSF","LowQualFinSF","GrLivArea","GarageArea",
               "WoodDeckSF","OpenPorchSF","EnclosedPorch","3SsnPorch","ScreenPorch","PoolArea",
               "MiscVal")

train.num <- select(train, one_of(cont_nums))
doPlots(train.num, fun = plotDen, ii =1:4, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =5:8, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =9:12, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =13:14, lab=log(train$SalePrice), ncol = 2)

train.cat <- select(train, one_of(cat.var))
doPlots(train.cat, fun = plotHist, ii = 1:6, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 7:12, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 13:18, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 19:24, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 25:30, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 31:36, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 37:42, lab=blank, ncol = 3)
doPlots(train.cat, fun = plotHist, ii = 43:46, lab=blank, ncol = 3)

# Plot sale price
library(scales)
ggplot(train, aes(x=SalePrice)) + geom_histogram(col = 'white') + theme_light() +scale_x_continuous(labels = comma)



