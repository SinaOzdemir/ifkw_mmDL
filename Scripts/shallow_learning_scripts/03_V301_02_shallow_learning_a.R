##########################################################
# Title:  V301_02 shallow classification                 #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","caret","here")

p_load(char = packs, install = T)

data<- readRDS(file = here::here("Data","shallow_learning_data","hot_encoding_ml.rds")) %>% 
  filter(!(doc_id %in% c("ecb-1202992784652808192","inea_eu-1235481777197604865"))) %>% 
  column_to_rownames(var = "doc_id")

data<- data %>% 
  mutate(V301_02 = as.factor(V301_02)) 

trainIndex<- createDataPartition(y = as.factor(data$V301_02),#when y is factor, create partition mimics the portions in the original dataset
                                 p = .8,
                                 list = F)

trainData<- data[trainIndex,]
testData<- data[-trainIndex,]

#roughly balanced outcomes 0:360 , 1:396 
table(trainData$V301_02)
#same as above 0:90 , 1:99 
table(testData$V301_02)

train_x<- trainData %>% select(-V301_02) %>% mutate(across(everything(),~as.numeric(.x)))
train_y<- trainData %>% select(V301_02) %>% mutate(V301_02 = as.factor(V301_02)) %>% pull()


# Naive bayes Output ------------------------------------------------------
req_pack<- c("naivebayes")
p_load(char = req_pack,install = T)
output_nb<- caret::train(y = train_y,
                         x = train_x,
                         metric = "Accuracy",
                         maximize = T,
                         method = "naive_bayes")

saveRDS(object = output_nb,file = here::here("Results","v301_02_nb.RDS"))


nb_pred_res<- predict(output_nb,newdata = testData,type = "raw")

nb_conf_mat<- confusionMatrix(data= nb_pred_res,reference=testData$V301_02, positive = "1")

nb_conf_res<-nb_conf_mat$byClass


# logistic regression -----------------------------------------------------

output_logreg<- caret::train(y = train_y,
                         x = train_x,
                         metric = "Accuracy",
                         maximize = T,
                         method = "glmnet")

saveRDS(object = output_logreg,file = here::here("Results","v301_02_logreg.RDS"))

lr_pred_res<- predict(output_logreg,newdata = testData,type = "raw")

lr_conf_mat<- confusionMatrix(data = lr_pred_res,reference = testData$V301_02,positive = "1")

lr_conf_res<- lr_conf_mat$byClass


# Support vector machine (Least Squares Support Vector Machine with Polynomial Kernel) --------------------------------------------------


req_packs<-c("kernlab")

p_load(char = req_packs,install = T)

levels(trainData$V301_02)<-c("No","Yes")


output_svmpoly<- caret::train(V301_02~.,
                              data= trainData,
                              metric = "Accuracy",
                              maximize = T,
                             method = "svmPoly")


saveRDS(object = output_svmpoly,file = here::here("Results","v301_02_svmPoly.RDS"))

svmpoly_pred<- predict(object = output_svmpoly,newdata = testData,type = "raw")

levels(testData$V301_02)<-c("No","Yes")

svm_conf_matrix<- confusionMatrix(data = svmpoly_pred,reference = testData$V301_02,positive = "Yes")

svm_conf_res<-svm_conf_matrix$byClass

# Random forest -----------------------------------------------------------
req_packs<-c("e1071", "ranger")

p_load(char = req_packs,install = T)

output_rf<- caret::train(y = train_y,
                         x = train_x,
                         metric = "Accuracy",
                         maximize = T,
                         method = "ranger")

saveRDS(object = output_rf,file = here("Results","v301_02_RF.rds"))


rf_pred<- predict(object = output_rf,newdata = testData,type = "raw")

levels(testData$V301_02)<-c("0","1")

rf_conf_mat<- confusionMatrix(data = rf_pred,reference = testData$V301_02,positive = "1")

rf_conf_res<- rf_conf_mat$byClass

# Alternative XGBOOS (xgbTree) --------------------------------------------


req_packs<-c("xgboost", "plyr")

p_load(char = req_packs,install = T)

output_xgbtree<- caret::train(y = train_y,
                              x = train_x,
                              metric = "Accuracy",
                              maximize = T,
                              method = "xgbTree")

saveRDS(object = output_xgbtree,file = here::here("Results","v301_02_xgbt.RDS"))

xgbt_pred<- predict(object = output_xgbtree,newdata = testData,type = "raw")

xgbt_conf<- confusionMatrix(xgbt_pred,testData$V301_02)

xgbt_conf_res<- xgbt_conf$byClass

ls() %>%
  grep(pattern = "_conf_res",x = .,value = T) %>%
  save(list = .,file = here::here("Results","dfm_shallow_learning_results.Rdata"))


