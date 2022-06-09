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

data<- readRDS(file = here("Data","shallow_learning_data","hot_encoding_ml.rds")) %>% 
  filter(!(doc_id %in% c("ecb-1202992784652808192","inea_eu-1235481777197604865"))) %>% 
  column_to_rownames(var = "doc_id")

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

saveRDS(object = output_nb,file = here("Results","v301_02_nb.rds"))
#Something is wrong; all the Accuracy metric values are missing:


# logistic regression -----------------------------------------------------

output_logreg<- caret::train(y = train_y,
                         x = train_x,
                         method = "glmnet")

saveRDS(object = output_logreg,file = here("Results","v301_02_logreg.rds"))

# Random forest -----------------------------------------------------------
req_packs<-c("e1071", "ranger")

p_load(char = req_packs,install = T)

output_rf<- caret::train(y = train_y,
                             x = train_x,
                             method = "ranger")

saveRDS(object = output_rf,file = here("Results","v301_02_RF.rds"))


# Support vector machine (Least Squares Support Vector Machine with Polynomial Kernel) --------------------------------------------------


req_packs<-c("kernlab")

p_load(char = req_packs,install = T)

output_svmlss<- caret::train(y = train_y,
                             x = train_x,
                             method = "lssvmPoly")

saveRDS(object = output_svmlss,file = here("Results","v301_02_svmlss.rds"))





# XGboost(DART boosting: https://xgboost.readthedocs.io/en/stable/tutorials/dart.html) -----------------------------------------------------------------


req_packs<-c("xgboost", "plyr")

p_load(char = req_packs,install = T)

output_xgbdart<- caret::train(y = train_y,
                         x = train_x,
                         method = "xgbDART")

saveRDS(object = output_xgbdart,file = here("Results","v301_02_xgbd.rds"))


# Alternative XGBOOS (xgbTree) --------------------------------------------


req_packs<-c("xgboost", "plyr")

p_load(char = req_packs,install = T)

output_xgbtree<- caret::train(y = train_y,
                              x = train_x,
                              method = "xgbTree")

saveRDS(object = output_xgbdart,file = here("Results","v301_02_xgbt.rds"))

