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

data<- readRDS(file = here::here("Data","shallow_learning_data","text_tfidf.rds")) %>% 
  rownames_to_column(var = "doc_id") %>% 
  filter(!(doc_id %in% c("ecb-1202992784652808192","inea_eu-1235481777197604865"))) 


dv<- readRDS(file = here::here("Data","shallow_learning_data","hot_encoding_ml.rds")) %>% 
  select(doc_id,V301_02) %>% 
  filter(!duplicated(doc_id))

data<- inner_join(data,dv,by = "doc_id") %>% 
  column_to_rownames("doc_id")

data<- data %>% 
  mutate(V301_02 = as.factor(V301_02)) 

trainIndex<- createDataPartition(y = as.factor(data$V301_02),#when y is factor, create partition mimics the portions in the original dataset
                                 p = .8,
                                 list = F)

trainData<- data[trainIndex,]
testData<- data[-trainIndex,]

#roughly balanced outcomes 0:304, 1:360
table(trainData$V301_02)
#same as above 0:75, 1:89
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

saveRDS(object = output_nb,file = here("Results","v301_02__tfidf_nb.rds"))

nb_pred<- predict(output_nb,testData,type ="raw")

nb_conf<- confusionMatrix(data = nb_pred,reference = testData$V301_02,positive = "1")

nb_res<-nb_conf$byClass

#Something is wrong; all the Accuracy metric values are missing:


# logistic regression -----------------------------------------------------

output_logreg<- caret::train(y = train_y,
                         x = train_x,
                         metric = "Accuracy",
                         maximize = T,
                         method = "glmnet")

saveRDS(object = output_logreg,file = here("Results","v301_02__tfidf_logreg.rds"))

logreg_pred<- predict(output_logreg,testData,type ="raw")

logreg_conf<- confusionMatrix(data = logreg_pred,reference = testData$V301_02,positive = "1")

logreg_res<-logreg_conf$byClass


# Support vector machine (Least Squares Support Vector Machine with Polynomial Kernel) --------------------------------------------------


req_packs<-c("kernlab")

p_load(char = req_packs,install = T)


output_svmpoly<- caret::train(V301_02~.,
                              data = trainData,
                              metric = "Accuracy",
                              maximize = T,
                              method = "svmPoly")



saveRDS(object = output_svmpoly,file = here("Results","v301_02__tfidf_svmpoly.rds"))


svm_pred<- predict(output_svmpoly,testData,type ="raw")

svm_conf<- confusionMatrix(data = svm_pred,reference = testData$V301_02,positive = "1")

svm_res<-svm_conf$byClass

# Random forest -----------------------------------------------------------
req_packs<-c("e1071", "ranger")

p_load(char = req_packs,install = T)

output_rf<- caret::train(y = train_y,
                             x = train_x,
                         metric = "Accuracy",
                         maximize = T,
                         method = "ranger")

saveRDS(object = output_rf,file = here("Results","v301_02__tfidf_RF.rds"))



rf_pred<- predict(output_rf,testData,type ="raw")

rf_conf<- confusionMatrix(data = rf_pred,reference = testData$V301_02,positive = "1")

rf_res<-rf_conf$byClass



# Alternative XGBOOS (xgbTree) --------------------------------------------


req_packs<-c("xgboost", "plyr")

p_load(char = req_packs,install = T)

output_xgbtree<- caret::train(y = train_y,
                              x = train_x,
                              metric = "Accuracy",
                              maximize = T,
                              method = "xgbTree")

saveRDS(object = output_xgbtree,file = here::here("Results","v301_02__tfidf_xgbt.rds"))


xgbt_pred<- predict(output_xgbtree,testData,type ="raw")

xgbt_conf<- confusionMatrix(data = xgbt_pred,reference = testData$V301_02,positive = "1")

xgbt_res<-xgbt_conf$byClass



# results -----------------------------------------------------------------
ls() %>% 
  grep(pattern = "_res",x = .,value = T) %>% 
  save(list = .,file = here::here("Results","tfidf_shallow_learning_results.Rdata"))