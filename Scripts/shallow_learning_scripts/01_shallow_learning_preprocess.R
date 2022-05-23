##########################################################
# Title:  Data preprocessing for shallow learning        #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","quanteda","here","fastText","qdapRegex","feather")

p_load(char = packs,install = T)


data<- readRDS(file = here("Data","labelled_data","DL_data.RDS"))

text_data<- 
# create_doc_id -----------------------------------------------------------

text_data <- data %>% ungroup() %>% 
  select(status_id,screen_name,text,V201:V301_06) %>% #select variables for analysis
  mutate(doc_id = paste0(screen_name,"-",status_id)) %>% #create unique document ids
  select(-status_id,-screen_name) %>% #drop unnecessary variables now
  mutate(text = str_remove_all(string = text,pattern = "\n")) %>% #remove line breaks from tweets
  mutate(text = str_remove_all(string = text,pattern = "\\p{So}|\\p{Cn}|\U0001f3fb|\U0001f3fc")) %>% #remove emojis from tweets
  mutate(text = qdapRegex::rm_twitter_url(text.var = text)) %>%  #remove urls from tweets
  mutate(text = str_remove_all(string = text,pattern = " &amp|:-&gt;|#|@"))#remove unrecognized characters, mentions and hashtags for language recognition

text<- text_data %>% pull(text)

text_data$text_lang<- fastText::language_identification(input_obj = text,
                                                        pre_trained_language_model_path = here("lang_recognition","lid.176.bin")) %>% #detect languages
  pull(iso_lang_1)

text_data<- text_data %>% filter(text_lang == "en") #keep only the english language tweets

# corpus ------------------------------------------------------------------

text_corpus<- text_data %>% 
  corpus(.,
         docid_field = "doc_id",
         text_field = "text")


text_tokens<- text_corpus %>% 
  tokens(remove_punct = T,remove_url = T) %>% #remove punctuations and links
  tokens_select(pattern = "#",selection = "remove",valuetype = "fixed") %>% #remove hashtags to simplfy
  tokens_select(pattern = "@",selection = "remove",valuetype = "fixed") %>% #remove mention symbols
  tokens_select(pattern = stopwords(language = "en",source = "snowball"),selection = "remove",valuetype = "fixed") %>% #remove stopwords
  tokens_wordstem(language = "en") %>% #stem words
  tokens_tolower() #decapitalize everything
  

text_dfm<- text_tokens %>% 
  dfm() %>% #basic document feature matrix with one-hot-encoding
  dfm_trim(min_docfreq =.1) %>% #remove extremely rare words, currently if it doesn't occur in 1% of documents we remove it
  convert(to = "data.frame") # convert to data.frame to use in caret for classification

text_tfidf<- text_tokens%>% 
  dfm() %>% 
  dfm_tfidf(scheme_tf = "count", #weight by term frequency to push rare words up
            scheme_df = "inverse") %>% #common inverse document frequency to push rare words furher up
  convert(to = "data.frame")#remove stopwords

text_dfm %>% write_feather(.,path = here("Data","shallow_learning_data","text_dfm.feather"))

text_tfidf %>%write_feather(.,path = here("Data","shallow_learning_data","text_tfidf.feather"))
 