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

packs<- c("tidyverse","ralger","here")

p_load(char = packs, install = T)

data_source<- here("Data","labelled_data")

data_dest <- here("Data","image_data")

dt<- readRDS(file = file.path(data_source,"DL_data.RDS")) %>% 
  filter(!duplicated(status_id))



# image scraper -----------------------------------------------------------

try(exp = {for (i in 1:nrow(dt)) {
  img_url<- pull(dt[i,"media_url"])
  img_ext<- tail(unlist(str_split(string = dt[i,"media_url"],pattern = "\\.",simplify = F)),n=1)
  img_name<- paste0(dt[i,"screen_name"],"-",dt[i,"status_id"],".",img_ext)
  img_path<- paste(data_dest,img_name,sep = "/")
  download.file(url = img_url,destfile = img_path,mode = "wb")
 
}},silent = T)
