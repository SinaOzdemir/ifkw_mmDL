# data migration from R to python:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","feather","reticulate"))

data_path<- here("data","shallow_learning_data")

if(!dir.exists(here("data","python_data"))){
  dir.create(here("data","python_data"))
}

data_dir<- list.files(path = data_path, pattern = "*.RDS",full.names = T)


# create feather ----------------------------------------------------------


for (i in 1:length(data_dir)) {
  cat("converting dataset", i, "\n")
  data_name<- gsub("C:/Users/sinaoz/OneDrive - NTNU/Projects/multimodal aca/data/shallow_learning_data/|.RDS","",data_dir[i])
  readRDS(data_dir[i]) %>% 
  write_feather(x = .,path = paste0(here("data","python_data"),"/",data_name,".feather"))
}

##test

data<- read_feather(path = here("data","python_data","bigram_dfm_shallow_learning_dt.feather"))
rm(data)


# reticulate csv ----------------------------------------------------------

if(isFALSE(virtualenv_exists(envname = here("maca")))){
  message("python environment is unavailabe, creating a new environment with python")
  reticulate::virtualenv_create(envname = here("maca"),
                                packages =c("pandas"))
  
}

reticulate::use_virtualenv(here("maca"))

pd<-import(module = "pandas")

for(i in 1:length(data_dir)){
  cat("converting dataset", i, "\n")
  data_name<- gsub("C:/Users/sinaoz/OneDrive - NTNU/Projects/multimodal aca/data/shallow_learning_data/|.RDS","",data_dir[i])
  save_path<- here("data","python_data",paste0(data_name,".csv"))
  data<-r_to_py(readRDS(data_dir[i]))
  data$to_csv(path_or_buf = save_path, header = TRUE)
  
}