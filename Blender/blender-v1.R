xgb_z <- read.csv("Submissions/xgb-custom-v6-1-10-17.csv")
glmnet_z <- read.csv("Submissions/caret-glmnet-v6-1-12-17.csv")
ranger_z <- read.csv("Submissions/ranger-mlr-v2-12-22-16.csv")

blender_orig <- cbind(xgb_z,glmnet_z$SalePrice,ranger_z$SalePrice)
View(blender_orig)

blender_orig$thirds_SalePrice <- (xgb_z$SalePrice+glmnet_z$SalePrice+ranger_z$SalePrice)/3

submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = blender_orig$thirds_SalePrice
write.csv(submission,file = 'Submissions/blender-thirds-v2-1-12-17.csv',row.names = FALSE)


blender_orig$fourths_SalePrice <- (xgb_z$SalePrice/4+glmnet_z$SalePrice/2+ranger_z$SalePrice/4)
submission$SalePrice = blender_orig$fourths_SalePrice
write.csv(submission,file = 'Submissions/blender-fourths-v2-1-12-17.csv',row.names = FALSE)

blender_orig$eights_SalePrice <- (2*xgb_z$SalePrice+5*glmnet_z$SalePrice+ranger_z$SalePrice)/8
submission$SalePrice = blender_orig$eights_SalePrice
write.csv(submission,file = 'Submissions/blender-eights-v1-1-11-17.csv',row.names = FALSE)

blender_orig$xgb_glmnet_SalePrice <- (4*xgb_z$SalePrice+4*glmnet_z$SalePrice)/8
submission$SalePrice = blender_orig$xgb_glmnet_SalePrice
write.csv(submission,file = 'Submissions/blender-xgb_glmnet-v2-1-12-17.csv',row.names = FALSE)
