###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature e
# Area based engineering

# Cut GrLiveArea
#hist(fullSet$GrLivArea, breaks=30)
fullSet$GrLivAreaCut<-as.factor(cut(fullSet$GrLivArea, c(0,1000,1200,1400,1600,1800,
                                                         2000,2500,3000,6000), right=FALSE, 
                                    labels=c(1:9)))