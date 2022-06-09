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

write_feather(x = dt,path = here("Data","labelled_data","DL_data.feather"))

# image scraper -----------------------------------------------------------


i=1

for (i in 1:nrow(dt)) {
  img_url<- pull(dt[i,"media_url"])
  img_ext<- tail(unlist(str_split(string = dt[i,"media_url"],pattern = "\\.",simplify = F)),n=1)
  img_name<- paste0(dt[i,"status_id"],".",img_ext)
  img_path<- paste(data_dest,img_name,sep = "/")
  
  img<- image_read(img_url)
  
  if(!is.null(img)){
    download.file(url = img_url,destfile = img_path,mode = "wb")}else{
      next
    }
  
} 

image_list<- list.files(path = here("Data","image_data"),all.files = T,no.. = T)  %>% gsub(pattern = ".jpg|.png",replacement = "",x = .)

dt_final<- dt %>% filter(status_id%in%image_list)

write_feather(x = dt_final,path = here("Data","labelled_data","DL_data.feather"))
