# Kaggle Housing Prices

# Feature engineering and results tracking test code

# Define list of features
# i.e. A, B, C, D
# these are features in addition to core code
# A - Basement variables
# B - Time-based changed...Decade built, new house, seasons, selling season
# C - Remodelling related
# D - First floor only indicator
# E - Area-based engineering - Cuts of GrLivArea
# F - GrLivArea / FirstFlrSF
# G - Reduce variables on certain categories with low representation
# H - Scale all numeric variables
# I - Adjust skewness of all numerica variable
# J - Reduce category variation
# K - Reduce zero, near zero variance
# L - Reduce multicollinarity of numeric variables
# M - OHE
# N - Geographical Features
# O - Crime Level

source("Code/1-load-data.R")
source("Code/2-1-eda.R")
# All features
 feature_list <- list(a=1, b=1, c=1, d=1, e=1, f=1, g=0, h=1, i=1, j=0, k=1, l=0, m=1, n=1, o=1)

# Scale and skewness, basement variables   #### No significant change
# feature_list <- list(a=1, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, time based change  #### No significant change
# feature_list <- list(a=0, b=1, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, remodelling related   #### No significant change
# feature_list <- list(a=0, b=0, c=1, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, first floor only   #### Negative influence
# feature_list <- list(a=0, b=0, c=0, d=1, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, area-based engineering   #### NO significant change
# feature_list <- list(a=0, b=0, c=0, d=0, e=1, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, area based    #### No significant change
#feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=1, g=1, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness, reduce variable with low rep   #### negative influence
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=1, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness  only   #### Positive inlfuence on rmse
#feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=0, o=0)

# Scale and skewness and near zero vriance #### Negative influence on rmse
#feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=1, l=0, m=0, n=0, o=0)

# Scale and skewness, reduce mutlicollinearity  #### no sig change
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=1, m=0, n=0, o=0)

# Scale and skewness, OHE #### Improvement on glmnet, negative inf on xgboost
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=1, n=1, o=1)

# Scale and skewness, geographical feature #### 
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=1, o=0)

# Scale and skewness, crime rate #### 
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=1, i=1, j=0, k=0, l=0, m=0, n=1, o=1)

# No features
# feature_list <- list(a=0, b=0, c=0, d=0, e=0, f=0, g=0, h=0, i=0, j=0, k=0, l=0, m=0, n=0, o=0)

# Create df with feature settings
resultsTable <- data.frame(feature_list)

# feature selection b
if (resultsTable$b == 1) {
  print("run this code")
  source("Code/3-feature-b.R")
}
# feature selection c
if (resultsTable$c == 1) {
  print("run this code")
  source("Code/3-feature-c.R")
}

# feature selection e
if (resultsTable$e == 1) {
  print("run this code")
  source("Code/3-feature-e.R")
}
# feature selection f
if (resultsTable$f == 1) {
  print("run this code")
  source("Code/3-feature-f.R")
}
# feature selection g
if (resultsTable$g == 1) {
  print("run this code")
  source("Code/3-feature-g.R")
}
# feature selection n
if (resultsTable$n == 1) {
  print("run this code")
  source("Code/3-feature-n.R")
}
# feature selection o 
if (resultsTable$o == 1) {
  print("run this code")
  source("Code/3-feature-o.R")
}



# Do this last since there are category reductions
# feature selection d
if (resultsTable$d == 1) {
  print("run this code")
  source("Code/3-feature-d.R")
}
# feature selection a
if (resultsTable$a == 1) {
  print("run this code")
  source("Code/3-feature-a.R")
}
# feature selection h
if (resultsTable$h == 1) {
  print("run this code")
  source("Code/3-feature-h.R")
}
# feature selection i
if (resultsTable$i == 1) {
  print("run this code")
  source("Code/3-feature-i.R")
}
# feature selection j
if (resultsTable$j == 1) {
  print("run this code")
  source("Code/3-feature-j.R")
}
# feature selection k
if (resultsTable$k == 1) {
  print("run this code")
  source("Code/3-feature-k.R")
}
# feature selection l
if (resultsTable$l == 1) {
  print("run this code")
  source("Code/3-feature-l.R")
}
# feature selection m
if (resultsTable$m == 1) {
  print("run this code")
  source("Code/3-feature-m.R")
}
