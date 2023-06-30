# DFM and TF-IDF preprocessing


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","quanteda","here","qdapRegex","devtools","emo"))

# data --------------------------------------------------------------------

dt<- readRDS(here("data","multimodal_aca_analysis_data.RDS"))

dt<- dt %>% select(screen_name,status_id,text)

tweet_languages<- readRDS(file = here("data","eu_data_011219_310720.RDS")) %>% 
  select(screen_name,status_id,lang) %>% 
  filter(screen_name%in%dt$screen_name & status_id%in%dt$status_id) %>% 
  distinct(status_id,.keep_all = T) %>% 
  filter(lang != "und") %>% 
  pull(lang) %>% 
  unique()

stop_words<-vector(mode = "character")

for (i in 1:length(tweet_languages)) {
  sw<- stopwords::stopwords(language = tweet_languages[i],source = "stopwords-iso")
  stop_words<-c(stop_words,sw)
}

emoji_rx<- emo::ji_rx

url_rx<- paste(qdapRegex:::reg_check(pattern = "@rm_twitter_url",dictionary = getOption("regex.library")),
               qdapRegex:::reg_check(pattern = "@rm_url",dictionary = getOption("regex.library")),
               qdapRegex:::reg_check(pattern = "@rm_url2",dictionary = getOption("regex.library")),
               sep = "|")


# preprocessing -----------------------------------------------------------

#remove emojis,url, and 0 length tweets
text_dt<- dt %>% 
  mutate(text = str_remove_all(pattern = emoji_rx,text)) %>% 
  mutate(text = str_remove_all(pattern = url_rx, text)) %>% 
  mutate(t_length = nchar(text)) %>% 
  filter(t_length >0) %>% 
  mutate(docid = paste0(screen_name,"_",status_id))

text_dfm<- text_dt %>% corpus(docid_field = "docid",text_field = "text") %>% 
  tokens(.,what = "word",
         remove_symbols = T) %>% 
  tokens_remove(stop_words) %>% 
  dfm() %>%
  dfm_select(pattern = "[[:digit:]]|[[:punct:]]",valuetype = "regex",min_nchar = 2,selection = "remove") %>% 
  dfm_trim(min_termfreq = 2) %>% 
  dfm_select(pattern = "amp",selection = "remove",valuetype = "fixed")
  

## dfm

text_dfm %>% 
  quanteda::convert(to = "data.frame",docid_field = "docid") %>% 
  saveRDS(.,file = here("data","shallow_learning_data","unigram_dfm_shallow_learning_dt.RDS"))


## dfm bi-gram  
text_dt %>% corpus(docid_field = "docid",text_field = "text") %>% 
  tokens(.,what = "word",
         remove_symbols = T) %>% 
  tokens_remove(stop_words) %>% 
  tokens_remove(pattern = "[[:digit:]]|[[:punct:]]",valuetype = "regex") %>% 
  tokens_select(min_nchar = 2,selection = "remove") %>% 
  tokens_remove("amp",valuetype = "fixed") %>% 
  tokens_ngrams(n = 2L) %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 2) %>% 
  convert(to = "data.frame",docid_field = "docid") %>% 
  saveRDS(.,file = here("data","shallow_learning_data","bigram_dfm_shallow_learning_dt.RDS"))

  


## unigram tf_idf


text_dfm %>%
  dfm_tfidf() %>% 
  quanteda::convert(to = "data.frame",docid_field = "docid") %>% 
  saveRDS(.,file = here("data","shallow_learning_data","unigram_tfidf_shallow_learning_dt.RDS"))


## bigram tf-idf


text_dt %>% corpus(docid_field = "docid",text_field = "text") %>% 
  tokens(.,what = "word",
         remove_symbols = T) %>% 
  tokens_remove(stop_words) %>% 
  tokens_remove(pattern = "[[:digit:]]|[[:punct:]]",valuetype = "regex") %>% 
  tokens_select(min_nchar = 2,selection = "remove") %>% 
  tokens_remove("amp",valuetype = "fixed") %>% 
  tokens_ngrams(n = 2L) %>% 
  dfm() %>%
  dfm_trim(min_termfreq = 2) %>% 
  dfm_tfidf() %>% 
  convert(to = "data.frame",docid_field = "docid") %>% 
  saveRDS(.,file = here("data","shallow_learning_data","bigram_tfidf_shallow_learning_dt.RDS"))

#should I add embedding script here?
# embedding ---------------------------------------------------------------


