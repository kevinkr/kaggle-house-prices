###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature G
# Simplify various categories
#
## Lot Shape
table(fullSet$LotShape)
# IR2 and Ir3 don't appear too often, combine into regular and irregular
levels(fullSet$LotShape)[levels(fullSet$LotShape)!="Reg"] <- "Irregular"

## LandContour
table(fullSet$LandContour)
# Most if level, simplify to Lvl and NotLvl
levels(fullSet$LandContour)[levels(fullSet$LandContour)!="Lvl"] <- "NotLvl"

## LandSlope
table(fullSet$LandSlope)
# Most are gentle, change others to NotGentle
levels(fullSet$LandSlope)[levels(fullSet$LandSlope)!="Gtl"] <- "NotGentle"

## Electrical
table(fullSet$Electrical)
# Most use Standard Breaker
levels(fullSet$Electrical)[levels(fullSet$Electrical)!="SBrkr"] <- "NotSBrkr"

## GarageType
table(fullSet$GarageType)
# Most have Attached or Detached or No Garage, combine others into Other
levels(fullSet$GarageType)[levels(fullSet$GarageType)=="2Types"] <- "OtherGarage"
levels(fullSet$GarageType)[levels(fullSet$GarageType)=="Basment"] <- "OtherGarage"
levels(fullSet$GarageType)[levels(fullSet$GarageType)=="CarPort"] <- "OtherGarage"

## PavedDrive
table(fullSet$PavedDrive)
# Most are Yes, all others called Unpaved
levels(fullSet$PavedDrive)[levels(fullSet$PavedDrive)!="Y"] <- "Unpaved"

## MiscFeature
table(fullSet$MiscFeature)
# Group all non-shed entries into Other
levels(fullSet$MiscFeature)[levels(fullSet$MiscFeature)=="Shed"] <- "Other"

## MSZoning 
table(fullSet$MSZoning)
# Group all non-residential entries into Other
levels(fullSet$MSZoning)[levels(fullSet$MSZoning)=="C (All)"] <- "Other"
levels(fullSet$MSZoning)[levels(fullSet$MSZoning)=="RH"] <- "Other"
