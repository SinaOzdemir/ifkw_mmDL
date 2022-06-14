#########################################################
# Title:  Image scraping                                #
# Author: Sina Özdemir                                  #
#         PhD candidate                                 #
#         Department of sociology and political science #
#         NTNU, Norway                                  #
#         sina.ozdemir@ntnu.no                          #
# Date: 05/05/2022                                      #
#########################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","magick","here","feather")

p_load(char = packs, install = T)

data_source<- here("Data","labelled_data")

data_dest <- here("Data","image_data")

dt<- readRDS(file = here("Data","labelled_data","DL_data.RDS")) %>% 
  drop_na()


text<- dt %>% pull(text)

tweet_lang<-vector(mode = "character",length = length(text))

for(i in 1:length(text)){
  cat("working on tweet ",i,"\n")
  lang_i<- fastText::language_identification(input_obj = text[i],
                                             pre_trained_language_model_path = here("lang_recognition","lid.176.bin")) %>% 
    filter(prob_1 == max(prob_1)) %>%
    pull(iso_lang_1)
  
  tweet_lang[i]<-lang_i
}

dt$lang<-tweet_lang

dt<-dt %>% filter(lang == "en")

# image scraper -----------------------------------------------------------

for (i in 1:nrow(dt)) {
  img_url<- dt[i,"media_url"]
  img_ext<- tail(unlist(str_split(string = dt[i,"media_url"],pattern = "\\.",simplify = F)),n=1)
  img_name<- paste0(dt[i,"status_id"],".",img_ext)
  img_path<- paste(data_dest,img_name,sep = "/")
  
  img<- image_read(img_url)
  
  if(!is.null(img)){
    download.file(url = img_url,destfile = img_path,mode = "wb")
    Sys.sleep(20)}
  else{
      next
    }
  
} 

image_list<- list.files(path = here("Data","image_data"),all.files = T,no.. = T)  %>% gsub(pattern = ".jpg|.png",replacement = "",x = .)

dt_final<- dt %>% filter(status_id%in%image_list)

dt_final<- dt_final %>% 
  mutate(text = qdapRegex::rm_twitter_url(text.var = text,clean = T)) %>% 
  filter(!duplicated(status_id))

write_feather(x = dt_final,path = here("Data","labelled_data","DL_data.feather"))
