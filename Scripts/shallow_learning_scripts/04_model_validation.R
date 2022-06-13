##V301_02 dfm feature test

library(pacman)


packs<- c("tidyverse","caret","here")

p_load(char = packs, install = T)

data<- readRDS(file = here::here("Data","shallow_learning_data","hot_encoding_ml.rds")) %>% 
  filter(!(doc_id %in% c("ecb-1202992784652808192","inea_eu-1235481777197604865"))) %>% 
  column_to_rownames(var = "doc_id")

data<- data %>% 
  mutate(V301_02 = as.factor(V301_02)) 


pred_cols<-data %>% select(-V301_02) %>% colnames(.)


# model train -------------------------------------------------------------

model_results<-list()
raw_results<- data.frame()

for (i in 1:1000) {
  
  message("Training and evaluation model: ",i)

  col_sample<- pred_cols[sample(1:length(pred_cols),size = (length(pred_cols)*.8),replace = F)]
  
  trainIndex<-createDataPartition(y= data$V301_02,p=.7,list = F)
  
  trainData<- data[trainIndex,] %>% select(V301_02,all_of(col_sample))
  testData<- data[-trainIndex,] %>% select(V301_02,all_of(col_sample))
  
  trainX<- trainData %>% select(-V301_02) %>% mutate(across(everything(),~as.numeric(.x)))
  trainY<- trainData %>% select(V301_02) %>% mutate(V301_02 = as.factor(V301_02)) %>% pull()
  
  
  logreg<- caret::train(y = trainY,
                        x = trainX,
                        method = "glmnet",
                        metric = "Accuracy",
                        maximize = T)
  
  train_res<- logreg$results %>%
    filter(Accuracy == max(Accuracy)) %>% 
    mutate(model_number = i)
  
  raw_results<-rbind(raw_results,train_res)
  
  log_pred<- predict(logreg,newdata =testData,type = "raw")
  
  log_conf_mat<- confusionMatrix(data = log_pred, reference = testData$V301_02,positive = "1")
  
  log_scores<- log_conf_mat$byClass
  
  model_results[[i]]<- log_scores
    
}


save(list = c(raw_results,model_results),file = here::here("Results","logreg_validation_results.Rdata"))

rf_model_results<-list()
rf_raw_results<- data.frame()

for (i in 1:1000) {
  
  message("Training and evaluation model: ",i)
  
  col_sample<- pred_cols[sample(1:length(pred_cols),size = (length(pred_cols)*.8),replace = F)]
  
  trainIndex<-createDataPartition(y= data$V301_02,p=.7,list = F)
  
  trainData<- data[trainIndex,] %>% select(V301_02,all_of(col_sample))
  testData<- data[-trainIndex,] %>% select(V301_02,all_of(col_sample))
  
  trainX<- trainData %>% select(-V301_02) %>% mutate(across(everything(),~as.numeric(.x)))
  trainY<- trainData %>% select(V301_02) %>% mutate(V301_02 = as.factor(V301_02)) %>% pull()
  
  
  logreg<- caret::train(y = trainY,
                        x = trainX,
                        method = "ranger",
                        metric = "Accuracy",
                        maximize = T)
  
  train_res<- logreg$results %>%
    filter(Accuracy == max(Accuracy)) %>% 
    mutate(model_number = i)
  
  rf_raw_results<-rbind(rf_raw_results,train_res)
  
  log_pred<- predict(logreg,newdata =testData,type = "raw")
  
  log_conf_mat<- confusionMatrix(data = log_pred, reference = testData$V301_02,positive = "1")
  
  log_scores<- log_conf_mat$byClass
  
  rf_model_results[[i]]<- log_scores
  
}

save(list = c(rf_raw_results,rf_model_results),file = here::here("Results","rf_validation_results.Rdata"))
