
# EDA Plotting ------------------------------------------------------------
# load previous code
#source("Code/2-1-eda.R")

###########################
## PLOTTING
########################
# continuous density plot
train.num <- select(train, one_of(cont_nums))
doPlots(train.num, fun = plotDen, ii =1:4, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =5:8, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =9:12, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =13:16, lab=log(train$SalePrice), ncol = 2)
doPlots(train.num, fun = plotDen, ii =17:18, lab=log(train$SalePrice), ncol = 2)

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

##### Examining categories in further detail
# review factor plots after reduction in labels
library(factoextra)
library(FactoMineR)
par(mfrow=c(4,3))
for (i in cat.var) {
  plot(fullSet[[i]], main=colnames(fullSet)[i],
       ylab = "Count", col="steelblue", las = 2)
}

# Explore Data Relationships
library(corrgram)
library(ellipse)
corrgram(fullSet,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pts,text.panel = panel.txt, cex=1.2)

# Missing data
require(Amelia)
missmap(fullSet, main="Train Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

DF = as.matrix(as.data.frame(lapply(fullSet, as.numeric)))

corrplot::corrplot(cor(DF), method = "circle", type = "lower", diag = FALSE,
                   order = "FPC", tl.cex = 0.6, tl.col = "black") #  plot matrix and ordered by first principal component
