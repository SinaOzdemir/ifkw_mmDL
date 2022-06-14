##########################################################
# Title:  Data preprocessing for shallow learning        #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------

require(pacman)

packs<- c("tidyverse","quanteda","here","qdapRegex","feather","quanteda.textstats")

p_load(char = packs,install = T)

labeled_data<- readRDS(file = here("Data","labelled_data","DL_data.RDS")) %>%
  ungroup() %>% select(status_id,screen_name,text,V301_02)

# combine tweet text and image text ---------------------------------------


# create_doc_id -----------------------------------------------------------

text_data <- labeled_data %>% 
  rename(feature_texts = text) %>% 
  mutate(doc_id = paste0(screen_name,"-",status_id)) %>% #create unique document ids
  select(-status_id,-screen_name) %>% #drop unnecessary variables now
  mutate(feature_texts = str_replace_all(string = feature_texts,pattern = "ı",replacement = "i")) %>% #
  mutate(feature_texts = str_remove_all(string = feature_texts,pattern = "\n")) %>% #remove line breaks from tweets
  mutate(feature_texts = qdapRegex::rm_twitter_url(text.var = feature_texts)) %>%  #remove urls from tweets
  mutate(feature_texts = str_remove_all(string = feature_texts,pattern = " &amp|&amp;|:-&gt;|#|@")) %>% #remove unrecognized characters, mentions and hashtags for language recognition
  mutate(feature_texts = str_remove_all(string = feature_texts, pattern = "[\U{1F1E6}-\U{1F1FF}-\U{1F300}-\U{1F64F}]|[\U{1F300}-\U{1F5FF}]|[\U{1F900}-\U{1F9FF}]|[\U{2700}-\U{27BF}]|[\U{1F100}-\U{1F1FF}]|[\U{2600}-\U{26FF}-\U{2935}]")) %>% 
  mutate(feature_texts = stringi::stri_trans_general(str = feature_texts,"latin-ascii")) %>% #should remove grapheme cluster character (non-latin letters)
  mutate(feature_texts = stringi::stri_trans_nfkc(str = feature_texts)) %>% #normalizes bold and italic characters created with unicode
  mutate(feature_texts = str_replace_all(string = feature_texts,pattern = ":",replacement = ".")) %>% 
  mutate(feature_texts = str_replace_all(string = feature_texts, pattern = "\\.", replacement = ". ")) %>% 
  drop_na() %>% 
  filter(!duplicated(doc_id))


label_balance<-as.data.frame(table(text_data$V301_02)) %>% rename(V301_02 = Var1)

write.table(x = label_balance,file = here("Data","shallow_learning_data","label_balance.csv"),sep = ";",row.names = F,col.names = T,fileEncoding = "UTF-8")

# corpus ------------------------------------------------------------------
##HERE##

text_corpus<- text_data %>% 
  corpus(.,
         docid_field = "doc_id",
         text_field = "feature_texts")


text_tokens<- text_corpus %>% 
  tokens(remove_punct = T,
         remove_url = T,
         remove_symbols = T,
         remove_numbers = T) %>% #remove punctuations and links
  tokens_select(pattern = "#",selection = "remove",valuetype = "fixed") %>% #remove hashtags to simplfy
  tokens_select(pattern = "@",selection = "remove",valuetype = "fixed") %>% #remove mention symbols
  tokens_select(pattern = stopwords(language = "en",source = "snowball"),
                selection = "remove",
                valuetype = "fixed") %>%#remove stop words
  tokens_select(pattern = "na|i|it",selection = "remove",valuetype = "regex") %>% #remove frequent noises
  tokens_wordstem(language = "en") %>% #stem words
  tokens_tolower() #decapitalize everything

text_dfm<- text_tokens %>% 
  dfm() #basic document feature matrix with hot encoding

feature_frequency<- textstat_frequency(x = text_dfm)  

feature_frequency_max<- feature_frequency %>%
  slice_max(n = 100,order_by = frequency) %>% 
  ggplot(aes(x = reorder(as.factor(feature),frequency),y = frequency))+
  geom_point()+
  theme_bw()+
  labs(x = "features",y = "frequencies")+
  coord_flip()

ggsave(feature_frequency_max,filename = "noise_detection.jpg",path = here("Graphs"),bg = "white")

text_dfm_df<- text_dfm %>% convert(to = "data.frame")#here is where the weirdness starts,

noise<- colnames(text_dfm)[which(nchar(colnames(text_dfm))<2)]

text_dfm_df<- text_dfm_df %>% 
  select(-all_of(x = noise)) %>% 
  column_to_rownames(var = "doc_id")

features<-data.frame(feat_names = colnames(text_dfm_df))

features<- features %>% 
  mutate(feature_id = seq.int(1,nrow(features),by = 1)) %>% 
  mutate(feature_tag = paste0("V",feature_id)) %>% 
  select(-feature_id)
  
colnames(text_dfm_df)<-features$feature_tag

saveRDS(features,file = here("Data","shallow_learning_data","hotencoding_feat_names.rds"))

saveRDS(object = text_dfm_df,file = here("Data","shallow_learning_data","hot_encoding_dfm.rds"))

# there are high frequency noises such as single digits, "na", signs "€" and single letters "ı","ıt" etc.
#this means that emoji cleaning and other stuff didn't exactly work as I hoped.
# should remove single length chars() from feautes too.

text_tfidf<- text_dfm_df %>%
  as.dfm() %>% 
  dfm_tfidf(scheme_tf = "count",#weight by term frequency to push rare words up
            scheme_df = "inverse") %>% #common inverse document frequency to push rare words further up
  convert(to = "data.frame") %>% 
  column_to_rownames(var = "doc_id")
    
text_tfidf %>%saveRDS(file = here("Data","shallow_learning_data","text_tfidf.rds"))
 