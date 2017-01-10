###########################################
# Kaggle Housing Prices
# Feature Engineering - 
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
  weak.prop.names <- reduce_cats(n, 0.05)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  fullSet[[n]] <- as.character(fullSet[[n]])
  fullSet[fullSet[[n]] %in% weak.prop.names, n] <- "OTHER"
  fullSet[[n]] <- as.factor(fullSet[[n]])
}