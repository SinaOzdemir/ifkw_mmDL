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


dv<- readRDS(file = here("Data","shallow_learning_data","hot_encoding_ml.rds")) %>% 
  select(doc_id,V301_02)

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
                         method = "naive_bayes")

saveRDS(object = output_nb,file = here("Results","v301_02__tfidf_nb.rds"))

a<- predict(output_nb,testData,type ="raw")

b<- confusionMatrix(data = a,reference = testData$V301_02,positive = "1")

c<-b$byClass

#Something is wrong; all the Accuracy metric values are missing:


# logistic regression -----------------------------------------------------

output_logreg<- caret::train(y = train_y,
                         x = train_x,
                         method = "glmnet")

saveRDS(object = output_logreg,file = here("Results","v301_02__tfidf_logreg.rds"))


# Support vector machine (Least Squares Support Vector Machine with Polynomial Kernel) --------------------------------------------------


req_packs<-c("kernlab")

p_load(char = req_packs,install = T)


output_svmpoly<- caret::train(V301_02~.,
                              data = trainData,
                              method = "svmPoly")



saveRDS(object = output_svmpoly,file = here("Results","v301_02__tfidf_svmpoly.rds"))


# Random forest -----------------------------------------------------------
req_packs<-c("e1071", "ranger")

p_load(char = req_packs,install = T)

output_rf<- caret::train(y = train_y,
                             x = train_x,
                             method = "ranger")

saveRDS(object = output_rf,file = here("Results","v301_02__tfidf_RF.rds"))






# XGboost(DART boosting: https://xgboost.readthedocs.io/en/stable/tutorials/dart.html) -----------------------------------------------------------------
# 
# 
# req_packs<-c("xgboost", "plyr")
# 
# p_load(char = req_packs,install = T)
# 
# output_xgbdart<- caret::train(y = train_y,
#                          x = train_x,
#                          method = "xgbDART")
# 
# saveRDS(object = output_xgbdart,file = here("Results","v301_02__tfidf_xgbd.rds"))
# 

# Alternative XGBOOS (xgbTree) --------------------------------------------


req_packs<-c("xgboost", "plyr")

p_load(char = req_packs,install = T)

output_xgbtree<- caret::train(y = train_y,
                              x = train_x,
                              method = "xgbTree")

saveRDS(object = output_xgbtree,file = here::here("Results","v301_02__tfidf_xgbt.rds"))



# results -----------------------------------------------------------------

results<- list.files(path = here("Results"),pattern = "tfidf",full.names = T)
model_name<- list.files(path = here("Results"),pattern = "tfidf",full.names = F)
f1<-function(model_path,test_data,type){
  model<- readRDS(model_path)
  model_pred<- predict(model,newdata = test_data, type = type)
  model_conf<- confusionMatrix(model_pred,reference = test_data$V301_02,positive = "1")
  model_results<- model_conf$byClass
  return(model_results)
}


model_results<-list()
for (i in 1:length(results)) {
  mf<- f1(model_path = results[i],test_data = testData,type = "raw")
  model_results[[i]]<-mf
  names(model_results)[i]<-model_name[i]
}

save(model_results,file = here::here("Results","tfidf_shallow_learning_results.Rdata"))
