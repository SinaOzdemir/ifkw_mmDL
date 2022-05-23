##########################################################
# Title: Shallow learning for text classification        #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------


require(pacman)

packs<- c("tidyverse","here","quanteda","quanteda.textmodels","caret")

p_load(char = packs, install = T)

labelled_data<- readRDS(file = here("Data","labelled_data","DL_data.RDS"))


# test --------------------------------------------------------------------

labelled_data_0<- labelled_data %>% filter(V301_01 == 0)
##k-fold validation should start here
labelled_data_1<- labelled_data %>%
  filter(V301_01 == 1) %>%ungroup() %>% 
  slice_sample(n = 74)

nb_test_data<- rbind(labelled_data_0,labelled_data_1)

text_data<- nb_test_data %>% select(status_id, text, V301_01)

tweet_corpus<- corpus(x = text_data,docid_field = "status_id",text_field = "text")

summary(tweet_corpus)
set.seed(190522)

train_id<- sample(1:nrow(nb_test_data),size = round(nrow(nb_test_data)*.8),replace = F)

tweet_corpus$id_numeric<- 1:ndoc(tweet_corpus)
#this is the most crucial step, right now I am going with some basics
tweets_dfm<- tweet_corpus %>% 
  tokens(remove_punct = T,remove_numbers = T) %>% 
  tokens_remove(pattern = stopwords("en")) %>% 
  tokens_wordstem() %>% 
  dfm()

tweet_dfm_training<- dfm_subset(x = tweets_dfm,subset = id_numeric%in%train_id)

tweet_dfm_test<- dfm_subset(x = tweets_dfm,subset = !(id_numeric%in%train_id))

tweet_nb<- textmodel_nb(tweet_dfm_training,y = tweet_dfm_training$V301_01,distribution = "Bernoulli")

summary(tweet_nb)
#ideas to improve pre-processing:
## 1) Remove links
## 2) replace mentions with a place holder (i.e. mention)
## 3) decapitalize everything etc.

dfm_matched <- dfm_match(tweet_dfm_test, features = featnames(tweet_dfm_training))

gt<- dfm_matched$V301_01

predicted<- predict(tweet_nb,newdata = dfm_matched)
class_tab<- table(gt,predicted)
test<- confusionMatrix(data = class_tab,mode = "everything",positive = "1")

test
