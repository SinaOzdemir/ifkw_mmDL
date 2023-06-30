#fit models


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","caret"))

options(expressions = 500000)
# data --------------------------------------------------------------------

dv_data<-readRDS(file = here("data","multimodal_aca_analysis_data.RDS")) %>% 
  select(screen_name,status_id,V301_02_Output) %>% 
  mutate(docid =paste(screen_name,status_id,sep = "_")) %>% 
  select(docid,V301_02_Output) %>% 
  rename(Output =V301_02_Output)

iv_data_dir<- list.files(path = here("data","shallow_learning_data"),
                         pattern = "*.RDS",full.names = T)

models_to_fit = c("nbDiscrete","glmnet",
                  "regLogistic",
                  "svmRadial",
                  "ranger",
                  "xgbTree")


# training ----------------------------------------------------------------


for (i in 1:length(iv_data_dir)) {
  
  data_name<- tail(unlist(str_split(string = iv_data_dir[i],pattern = "/",simplify = F)),n = 1) %>% 
    str_remove(.,"_shallow_learning_dt.RDS")
  
  cat("training shallow learners with", data_name,"\n")
  
  ivs<- readRDS(iv_data_dir[i]) %>% 
    column_to_rownames("docid")
  
  iv_pseudonyms<- paste0("v",seq.int(1,length(colnames(ivs)),by = 1))
  
  names(iv_pseudonyms)<- colnames(ivs)
  
  saveRDS(iv_pseudonyms,file = here("data","shallow_learning_data","pseudonyms",paste0(data_name,"_pseudonyms.RDS")))
  
  colnames(ivs)<-iv_pseudonyms
  
  ivs<- ivs %>% 
    rownames_to_column(var = "docid") %>% 
    left_join(.,dv_data,by = "docid") %>% 
    column_to_rownames(var = "docid") %>% 
    mutate(Output = as.factor(recode(Output, `1` = "yes", `0` = "no")))
  
  train_test<- createDataPartition(y = ivs$Output,p = .8, list = F)
  
  training_data<- ivs[train_test,]
  test_x<- ivs[-train_test,] %>% select(-Output)
  test_y<- ivs[-train_test,] %>% pull(Output)
  
  model_results<-list()
  model_performance<-list()
  
  for (j in 1:1) {
    
    method<- models_to_fit[j]
    
    cat("training ", method," using ", data_name, "\n")
    
    if(method == "glmnet"){
    train_control<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 10,
                                 search = "random",
                                 p = .7,
                                 verboseIter = T,
                                 classProbs = T,
                                 allowParallel = T)
    
  }else{
    train_control<- trainControl(method = "repeatedcv",
                                 number = 5,
                                 repeats = 5,
                                 search = "grid",
                                 p = .7,
                                 verboseIter = T,
                                 classProbs = T,
                                 allowParallel = T)
  }
    #Error: protect(): protection stack overflow: ngrams have too many variables
    
    model<- train(form = Output~.,
                  data = training_data,
                  method = method,
                  metric = "Accuracy",
                  trControl = train_control)
    
    model_results[[j]]<- model
    
    names(model_results)[j]<- method
    
    model_pred<- predict(model, test_x, type = "raw")
    
    model_performance[[j]]<- confusionMatrix(model_pred,test_y,mode = "everything")$byClass
    names(model_performance)[j]<- method
  
  }
  
  save(model_results,model_performance, file = here("Results","shallow_learners",paste0(data_name,".Rdata")))}
