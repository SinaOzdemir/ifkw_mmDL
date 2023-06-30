# Deep learning classification

# setup -------------------------------------------------------------------

library(pacman)
p_load(char = c("tidyverse","here","keras"))

data_dirs<- list.files(path = here("data","shallow_learning_data"),pattern = "*.RDS",full.names = T)


dv_data<-readRDS(file = here("data","multimodal_aca_analysis_data.RDS")) %>% 
  select(screen_name,status_id,V301_02_Output) %>% 
  mutate(docid =paste(screen_name,status_id,sep = "_")) %>% 
  select(docid,V301_02_Output) %>% 
  rename(Output =V301_02_Output)

set.seed(121314)
train_test_index<- caret::createDataPartition(y = dv_data$Output,p = .8,list = F)

validation_index<-sample(x = train_test_index,size = (length(train_test_index)*.2),replace = F)

train_test_index<-train_test_index[-validation_index]

# modelling ---------------------------------------------------------------

for (i in 1:length(data_dirs)) {
  print("data stuff")
  ###----
  data_name<- list.files(path = here("data","shallow_learning_data"),pattern = "*.RDS",full.names = F)[i] %>% 
    gsub("_shallow_learning_dt.RDS","",.)
  
  save_name<- paste0(data_name,"_dl",".Rdata")
  
  data<-readRDS(file = data_dirs[i]) %>%
    left_join(dv_data,.,by = "docid") %>% 
    select(-docid) %>%
    mutate(across(everything(),~as.numeric(.x))) %>% 
    as.matrix()
  
  x_train<-data[train_test_index,colnames(data)!="Output"]
  y_train<- data[train_test_index,colnames(data)=="Output"]
  
  x_test<-data[-train_test_index,colnames(data)!="Output"]
  y_test<-data[-train_test_index,colnames(data)=="Output"]
  
  
  x_validation<- data[validation_index,colnames(data)!="Output"]
  y_validation<- data[validation_index,colnames(data)=="Output"]
  
  print("model build")  
  #----
  model<-keras_model_sequential() %>% 
    layer_dense(units = 50,activation = "relu",input_shape = ncol(x_train)) %>% 
    layer_dense(units = 40,activation = "relu") %>% 
    layer_dropout(rate = .5) %>% 
    layer_dense(units = 30,activation = "relu") %>% 
    layer_dense(units = 20,activation = "relu") %>% 
    layer_dropout(rate = .5) %>% 
    layer_dense(units = 10,activation = "relu") %>% 
    layer_dense(units = 5,activation = "relu") %>% 
    layer_dense(units = 1,activation = "sigmoid")
  
  opt = keras::optimizer_adam(learning_rate = 0.001)
  
  model %>% compile(optimizer = opt,
    loss = "binary_crossentropy",
    metrics = "accuracy")  
    
  early_stop_monitor = keras::callback_early_stopping(monitor = "val_accuracy",
                                                      min_delta = 0,
                                                      patience = 10,
                                                      verbose = 0,
                                                      mode = "auto",
                                                      baseline = NULL,
                                                      restore_best_weights = T)
  
  print("model fit")
  #----
  model_hist<- model %>% 
    fit(x_train,y_train,
        batch_size = (nrow(x_train)/50),
        epochs = 50,
        validation_data = list(x_validation,y_validation)
        ,callbacks = early_stop_monitor)
  
  model_name<- paste0(data_name,"_dense_sequential.hdf5")
  
  keras::save_model_hdf5(model,filepath = here("Results","deep_learner",model_name))     
  print("model evaluation")
  #----
  model_predictions<- model %>% predict(.,x_test) %>% `>`(.8) %>% as.vector(mode = "integer") %>% as.factor()
    
  model_performance_karet<- caret::confusionMatrix(model_predictions,as.factor(y_test),positive = "1")$byClass
  
  save(model_hist,model_performance_karet,
        file = here("Results","deep_learner",save_name))
}


