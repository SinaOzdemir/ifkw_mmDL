#########################################################
# Title: Scraping external tweets                       #
# Author: Sina Özdemir                                  #
#         PhD candidate                                 #
#         Department of sociology and political science #
#         NTNU, Norway                                  #
#         sina.ozdemir@ntnu.no                          #
#########################################################


# setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","academictwitteR")

p_load(char = packs)

bearer_token<-"AAAAAAAAAAAAAAAAAAAAANh3NgEAAAAARyR94ftEBLuHRcn%2BeanwQkaUn9Y%3DZGq3hWKCdHByAU6stgrxazCG0XOzsqhxDFI2GffT8sIIk1Cz19"

# data --------------------------------------------------------------------

tweets_to_download<- readRDS(file = here("data","external_tweets_to_collect.RDS"))

tweets_to_hydrate <- tweets_to_download %>% pull(source_status_id)


#(DO NOT RERUN) tweet_scraping ----------------------------------------------------------


external_tweets<-academictwitteR::hydrate_tweets(ids = tweets_to_hydrate,
                                bearer_token = bearer_token,
                                bind_tweets = T,
                                errors = T)

saveRDS(external_tweets,file = here("data","hydrated_external_tweets.RDS"))

## native hydrate_tweets do not fetch media_url (link to the image to scrape)
## media url comes with attachements,includes media,media fields == url
  ## There are three possible alternatives
      #1) modify the source code of hydrate tweets to fetch media_url field(source code:https://github.com/cjbarrie/academictwitteR/blob/master/R/hydrate_tweets.R)
      #2) Communicate with the API v2 directly to fetch media_url field for tweets
      #3) Scrape media_url from the tweet link with dynamic website scraping with RSelenium.


## sample media fields request


#curl --request GET
#'https://api.twitter.com/2/tweets?ids=1263145271946551300&expansions=attachments.media_keys&media.fields=duration_ms,height,media_key,preview_image_url,public_metrics,type,url,width,alt_text'
#--header 'Authorization: Bearer $BEARER_TOKEN'

#honestly communciating with api seems rather easy

# clean up the data -------------------------------------------------------

ext_tweets<- readRDS(file= here("data","hydrated_external_tweets.RDS"))

ext_img<- ext_tweets %>% select(id,author_id,attachments)

attachments<- ext_tweets$attachments$media_keys

media_keys_vecto<- vector(mode = "character",length = length(attachments))

for (i in 1:length(attachments)){
  twt_<- attachments[[i]]
  
  if (is.null(twt_)){
    media_keys_vecto[i]<-NA
  }else{
    media_keys_vecto[i] <- twt_
  }
  
  
}

clean_tweets<- data.frame(status_id = ext_tweets$id,
                          user_id = ext_tweets$author_id,
                          media_key = media_keys_vecto) %>% 
  drop_na(media_key)

saveRDS(object = clean_tweets, file = here("data","ext_img_tweets.RDS"))
# build twitter querries --------------------------------------------------


get_media<- function(status_id, media_key, field.params = "url", auth){
  api_core = "https://api.twitter.com/2/tweets?ids="
  
  expansion_arg = "expansions=attachments.media_keys&media.fields="
  
  api_status_call = paste0(api_core,status_id)
  if (length(field.params)>1){
    field.params = paste(field.params,collapse = ",")
  }
  api_image_call = paste0(expansion_arg,field.params)
  
  api_call<- paste0(api_status_call,"&",api_image_call)
  
  #authenticate the call 
  ##need to look up how to make a curl call from R(there was something on twitter github page)
  
  check_bearer <- function(bearer_token){
    if(missing(bearer_token)){
      stop("bearer token must be specified.")
    }
    if(substr(bearer_token,1,7)=="Bearer "){
      bearer <- bearer_token
    } else{
      bearer <- paste0("Bearer ",bearer_token)
    }
    return(bearer)
  }
  
  bearer_token <- check_bearer(auth)
  #query = params??
  #get can build api requests with parameters if supplied with "query" argument
  #in the future, might be better to functionize parameter specification
  api_response<-httr::GET(url = api_call, httr::add_headers(Authorization = bearer_token))
  
  json_from_api<- jsonlite::fromJSON(httr::content(api_response, "text"))
  
  json_contents<- names(json_from_api)
  
  if(("includes"%in%json_contents)){
    includes<- json_from_api[["includes"]]
    includes_content<- names(includes)
    
    if(("media"%in% includes_content)){
      media_info<- json_from_api[["includes"]][["media"]] %>% 
        as.data.frame() %>% 
        mutate(status_id = status_id)
      rownames(media_info)<- NULL
      return(media_info)
      
      
    }else{
      warning("API response does not contain information on media content")
      if(!file.exists(paste0(getwd(),"/missing_media_info.txt"))){
        file.create(paste0(getwd(),"/missing_media_info.txt"))
        cat(as.character(status_id),file = paste0(getwd(),"/missing_media_info.txt"),sep = "\n",append = T)
        }else{
        cat(as.character(status_id),file = paste0(getwd(),"/missing_media_info.txt"),sep = "\n",append = T)
      }
    }
  }else{
    warning("API response lacks further information on the tweet")
    if(!file.exists(paste0(getwd(),"/missing_info.txt"))){
      file.create(paste0(getwd(),"/missing_info.txt"))
      cat(as.character(status_id),file = paste0(getwd(),"/missing_info.txt"),sep = "\n",append = T)
    }else{
      cat(as.character(status_id),file = paste0(getwd(),"/missing_info.txt"),sep = "\n",append = T)
    }
    return(json_from_api)
  }
}

#function test
# 
# status_id<-twt_to_scrape$status_id[1]
# media_key<- twt_to_scrape$media_key[1]
# media_params<- c("duration_ms","height","media_key","preview_image_url","public_metrics","type","url","width","alt_text")
# 
# 
# function_test<- get_media(status_id = status_id,
#                           media_key = media_key,
#                           auth = bearer_token)

#it would be great if I flatten the json to return: status_id, media_key, media_urls


# scrape the image info ---------------------------------------------------

twt_to_scrape<- readRDS(file = here("data","ext_img_tweets.RDS"))

## rbind has a problem with row names
## "data" and "includes[media]" have differing rows sometimes so 
## cbind doesn't work properly, instead should just add the status id 
## from the arguments to the "includes[media]" data with mutate
## also issue a warning or stop if "includes[media]" doesn't have image url.
tweet_media_info<- data.frame()

for(i in 1:nrow(twt_to_scrape)){
  cat("getting info on tweet", i,"\n")
  
  if (i%%100 == 0) {
    cat("letting API rest\n")
    
    Sys.sleep(time = (60*20))
    
  }
  
  twt_media_info<- get_media(status_id = twt_to_scrape$status_id[i],
                             media_key = twt_to_scrape$media_key[i],
                             auth = bearer_token)
  
  #if the returned stuff is json save json, if it dataframe do something else
  #directory problem migt be because I am wrking in usb stick
  if(!is.data.frame(twt_media_info)){
    cat("object is not data frame, saving json\n")
    if(!dir.exists(here("json_files"))){
      dir.create(path = here("json_files"))
      saveRDS(object = twt_media_info,file = paste0(here("json_file"),"/json_",twt_to_scrape$status_id[i],".RDS"))
    }else{
      saveRDS(object = twt_media_info,file = paste0(here("json_file"),"/json_",i,".RDS"))
    }
  }

  if(is.data.frame(twt_media_info)){
  if("url"%in%colnames(twt_media_info)){
  tweet_media_info<-rbind(tweet_media_info,twt_media_info)
  }else{
    cat("tweet does not contain image url\n")
    if(!file.exists(paste0(getwd(),"/missing_url.txt"))){
      file.create(paste0(getwd(),"/missing_url.txt"))
      cat(as.character(twt_to_scrape$status_id[i]),file = paste0(getwd(),"/missing_url.txt"),sep = "\n",append = T)
    }else{
      cat(as.character(twt_to_scrape$status_id[i]),file = paste0(getwd(),"/missing_url.txt"),sep = "\n",append = T)
    }
    next
  }}
}

saveRDS(tweet_media_info,file = here("data","external_tweet_image_urls.RDS"))


# build the scraping list -------------------------------------------------

images_to_scrape<- readRDS(file = here("data","images_to_scrape.RDS"))

external_tweets<- readRDS(file = here("data","external_tweets_to_collect.RDS"))

external_tweet_image_url<- readRDS(file = here("data","external_tweet_image_urls.RDS"))

external_to_labeled<- left_join(external_tweet_image_url,external_tweets,by = c("status_id" = "source_status_id")) %>% 
  select(screen_name,status_id,url) %>% 
  rename(media_url = url)

full_data<- readRDS(here("data","eu_data_011219_310720.RDS")) %>% filter(status_id %in% images_to_scrape$status_id) %>% 
  select(screen_name,status_id,media_url)

image_list<- rbind(full_data,external_to_labeled)

final_image_list<- image_list %>% 
  filter(!grepl(pattern = "video",x = media_url))

saveRDS(final_image_list,file = here("data","final_image_list.RDS"))
# labeles and text -----------------------------------------------------------------

image_list<- readRDS(file = here("data","final_image_list.RDS"))

labels<- readRDS(file = here("data","analysis_data_11042022.RDS")) %>% 
  select(screen_name,status_id,V301_02_Output) %>% 
  distinct(status_id,.keep_all = T)

text<- readRDS(file = here("data","eu_data_011219_310720.RDS")) %>% 
  select(screen_name,status_id,text) %>% 
  filter(screen_name%in%labels$screen_name & status_id %in% labels$status_id)


analysis_data<- left_join(labels,image_list, by = c("screen_name","status_id")) %>% 
  left_join(.,text, by = c("screen_name","status_id")) %>% 
  distinct(status_id,.keep_all = T) %>% 
  mutate(media_url = as.character(media_url)) %>% 
  mutate(image_name = str_remove_all(pattern = "http://pbs.twimg.com/media/",string = media_url)) %>% 
  mutate(image_name = as.character(image_name))

analysis_data$media_url<- ifelse(analysis_data$media_url=="NULL",NA,dt$media_url)
analysis_data$image_name<- ifelse(analysis_data$image_name == "NULL",NA,dt$image_name)


saveRDS(analysis_data,file = here("data","multimodal_aca_analysis_data.RDS"))

install.packages("feather")
library(feather)

feather::write_feather(x = analysis_data,path = here("data","multimodal_aca_analysis_data.feather"))
