# House Prices: Advanced Regression Techniques 
# Deadline March 1, 2017

options(scipen=999) # remove scientific notation

#Load data
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}

housing_data.path <- "/Data/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")

train.column.types <- c('integer',   # Id
                        'factor' ,   # MSSubClass
                        'factor',    # MSZoning
                        'integer',   # LotFrontage
                        'integer',   # LotArea
                        'factor',    # Street
                        'factor',    # Alley
                        'factor',    # LotShape                       
                        
                        
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)