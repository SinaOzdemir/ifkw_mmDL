##########################################################
# Title:  Data preprocessing for shallow learning        #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","quanteda","here","fastText","qdapRegex","feather","quanteda.textstats")

p_load(char = packs,install = T)

labeled_data<- readRDS(file = here("Data","labelled_data","DL_data.RDS")) %>% ungroup() %>%

image_texts<- readRDS(file = here("Data","shallow_learning_data","image_texts.rds"))


# combine tweet text and image text ---------------------------------------

image_texts<- image_texts %>% 
  mutate(image_id = str_remove_all(string = image_name,pattern = ".jpg")) %>% 
  mutate(status_id = str_split(string = image_id,pattern = "-",simplify = T)[,2]) %>% 
  mutate(screen_name = str_split(string = image_id,pattern = "-",simplify = T)[,1]) %>% 
  rename(image_text = text)

sl_data<- left_join(labeled_data,image_texts,by = c("status_id","screen_name"))

sl_data_text<- sl_data %>% 
  group_by(status_id,screen_name) %>% 
  summarise(feature_texts = paste(text,image_text,collapse = "."))

sl_data_b<- left_join(sl_data,sl_data_text, by = c("screen_name","status_id")) %>% 
  select(screen_name,status_id,feature_texts, matches(match = "V301_*"))


# create_doc_id -----------------------------------------------------------

text_data <- sl_data_b %>% 
  select(status_id,screen_name,feature_texts,matches(match = "V301_*")) %>% #select variables for analysis
  mutate(doc_id = paste0(screen_name,"-",status_id)) %>% #create unique document ids
  select(-status_id,-screen_name) %>% #drop unnecessary variables now
  mutate(feature_texts = str_remove_all(string = feature_texts,pattern = "\n")) %>% #remove line breaks from tweets
  mutate(feature_texts = str_remove_all(string = feature_texts,pattern = "\\p{So}|\\p{Cn}|\U0001f3fb|\U0001f3fc")) %>% #remove emojis from tweets
  mutate(feature_texts = qdapRegex::rm_twitter_url(text.var = feature_texts)) %>%  #remove urls from tweets
  mutate(feature_texts = str_remove_all(string = feature_texts,pattern = " &amp|:-&gt;|#|@")) %>% #remove unrecognized characters, mentions and hashtags for language recognition
  drop_na()

text<- text_data %>% pull(feature_texts)

text_data$text_lang<- fastText::language_identification(input_obj = text,
                                                        pre_trained_language_model_path = here("lang_recognition","lid.176.bin")) %>% #detect languages
  pull(iso_lang_1)

text_data<- text_data %>% filter(text_lang == "en") #keep only the english language tweets

label_balance<- text_data %>% 
  select(matches("V301_*")) %>% 
  pivot_longer(cols = everything(),names_to = "labels",values_to = "presence") %>% 
  group_by(labels,presence) %>% 
  summarise(label_count = n()) %>% 
  pivot_wider(names_from = "presence",values_from = "label_count")

write.table(x = label_balance,file = here("Data","shallow_learning_data","label_shares.csv"),sep = ";",quote = T,row.names = F,col.names = T,fileEncoding = "UTF-8")

# corpus ------------------------------------------------------------------

text_corpus<- text_data %>% 
  corpus(.,
         docid_field = "doc_id",
         text_field = "feature_texts")


text_tokens<- text_corpus %>% 
  tokens(remove_punct = T,remove_url = T) %>% #remove punctuations and links
  tokens_select(pattern = "#",selection = "remove",valuetype = "fixed") %>% #remove hashtags to simplfy
  tokens_select(pattern = "@",selection = "remove",valuetype = "fixed") %>% #remove mention symbols
  tokens_select(pattern = stopwords(language = "en",source = "snowball"),selection = "remove",valuetype = "fixed") %>% #remove stop words
  tokens_wordstem(language = "en") %>% #stem words
  tokens_tolower() #decapitalize everything
  

text_dfm<- text_tokens %>% 
  dfm() #basic document feature matrix with one-hot-encoding

feature_frequency<- textstat_frequency(x = text_dfm)  

feature_frequency_max<- feature_frequency %>%
  slice_max(n = 100,order_by = frequency) %>% 
  ggplot(aes(x = reorder(as.factor(feature),frequency),y = frequency))+
  geom_point()+
  theme_bw()+
  coord_flip()
# there are high frequency noises such as single digits, "na", signs "€" and single letters "ı","ıt" etc.
    
text_tfidf<- text_tokens%>% 
  dfm() %>% 
  dfm_tfidf(scheme_tf = "count", #weight by term frequency to push rare words up
            scheme_df = "inverse") %>% #common inverse document frequency to push rare words furher up
  convert(to = "data.frame")#remove stopwords

text_dfm %>% write_feather(.,path = here("Data","shallow_learning_data","text_dfm.feather"))

text_tfidf %>%write_feather(.,path = here("Data","shallow_learning_data","text_tfidf.feather"))
 