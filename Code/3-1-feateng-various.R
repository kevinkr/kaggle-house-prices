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
# 
# to-do list
# OHE
# nzv
# insignifcant category variables

feature_list <- list(a=1, b=1, c=1, d=1, e=1, f=1, g=1)

# Create df with feature settings
resultsTable <- data.frame(feature_list)

# feature selection example
if (resultsTable$b == 1) {
  print("run this code")
  source("Code/3-feature-b.R")
}
# feature selection example
if (resultsTable$c == 1) {
  print("run this code")
  source("Code/3-feature-c.R")
}

# feature selection example
if (resultsTable$e == 1) {
  print("run this code")
  source("Code/3-feature-e.R")
}
# feature selection example
if (resultsTable$f == 1) {
  print("run this code")
  source("Code/3-feature-f.R")
}
# feature selection example
if (resultsTable$g == 1) {
  print("run this code")
  source("Code/3-feature-g.R")
}


# Do this last since there are category reductions
# feature selection example
if (resultsTable$d == 1) {
  print("run this code")
  source("Code/3-feature-d.R")
}
# feature selection example
if (resultsTable$a == 1) {
  print("run this code")
  source("Code/3-feature-a.R")
}

# GRLiveArea - zScore
#fullSet$GrLivAreaZscore <- scale(fullSet$GrLivArea, center = TRUE, scale = TRUE)[,]
#fullSet <- subset(fullSet, select = -c(GrLivArea))
