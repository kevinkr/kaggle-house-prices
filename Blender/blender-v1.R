xgb_z <- read.csv("Submissions/xgb-custom-v11-1-26-17.csv")
glmnet_z <- read.csv("Submissions/caret-glmnet-v15-1-27-17.csv")
ranger_z <- read.csv("Submissions/ranger-mlr-v2-12-22-16.csv")
svmlinear_Z <- read.csv("Submissions/caret-svmlinear-v2-1-25-17.csv")
bayesblm_z <- read.csv("Submissions/caret-bayesglm-v2-1-25-17.csv")
glmboost_z <- read.csv("Submissions/caret-glmboost-v2-1-30-17.csv")


blender_orig <- cbind(xgb_z,glmnet_z$SalePrice,
                      svmlinear_Z$SalePrice, bayesblm_z$SalePrice,
                      glmboost_z$SalePrice)
View(blender_orig)

blender_orig$equaldiv <- rowSums(blender_orig[,-1])/(ncol(blender_orig)-1)

submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = blender_orig$equaldiv
write.csv(submission,file = 'Submissions/equaldiv-v2-1-25-17.csv',row.names = FALSE)
# PL .12941

blender_orig$fourths_SalePrice <- (xgb_z$SalePrice/4+glmnet_z$SalePrice/2+bayesblm_z$SalePrice/4)
submission$SalePrice = blender_orig$fourths_SalePrice
write.csv(submission,file = 'Submissions/blender-fourths-v3-1-25-17.csv',row.names = FALSE)


blender_orig$eights_SalePrice <- (2*xgb_z$SalePrice+5*glmnet_z$SalePrice+bayesblm_z$SalePrice)/8
submission$SalePrice = blender_orig$eights_SalePrice
write.csv(submission,file = 'Submissions/blender-eights-v1-1-11-17.csv',row.names = FALSE)

# best submission 1-27-17
blender_orig$xgb_glmnet_SalePrice <- (2*xgb_z$SalePrice+6*glmnet_z$SalePrice)/8
submission$SalePrice = blender_orig$xgb_glmnet_SalePrice
write.csv(submission,file = 'Submissions/blender-xgb2_glmnet6-v1-1-27-17.csv',row.names = FALSE)


blender_orig$xgb_glmnet_SalePrice <- (1*xgb_z$SalePrice+7*glmnet_z$SalePrice)/8
submission$SalePrice = blender_orig$xgb_glmnet_SalePrice
write.csv(submission,file = 'Submissions/blender-xgb1_glmnet7-v2-1-28-17.csv',row.names = FALSE)

blender_orig$glmboost_glmnet_SalePrice <- (4*glmboost_z$SalePrice+4*glmnet_z$SalePrice)/8
submission$SalePrice = blender_orig$glmboost_glmnet_SalePrice
write.csv(submission,file = 'Submissions/blender-4glmboost_4glmnet-v3-1-30-17.csv',row.names = FALSE)

blender_orig$glmboost_glmnet_SalePrice <- (6*glmboost_z$SalePrice+2*glmnet_z$SalePrice)/8
submission$SalePrice = blender_orig$glmboost_glmnet_SalePrice
write.csv(submission,file = 'Submissions/blender-6glmboost_2glmnet-v3-1-30-17.csv',row.names = FALSE)
