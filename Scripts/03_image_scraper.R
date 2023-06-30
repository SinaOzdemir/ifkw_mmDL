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

p_load(char = c("tidyverse","magick","here","feather"))

make.dir<- function(path,name){
  dir_path<- paste0(path,"/",name)
  if(dir.exists(dir_path)){
    return(dir_path)
  }else{
    dir.create(dir_path)
    return(dir_path)
  }
  
}

data_dest <- make.dir(path = here("data"),name = "image_data")

dt<-readRDS(file = here("data","multimodal_aca_analysis_data.RDS"))


# image scraper -----------------------------------------------------------

#fix NULL as character


for (i in 1:nrow(dt)) {
  cat("scraping images for tweet ",dt[i,"status_id"],"\n")
  
  img_url<- dt[i,"media_url"]
  img_name<- dt[i,"image_name"]
  img_dest<- paste0(data_dest,"/",img_name)
  
  if(is.na(img_url)){
    cat("tweets doesn't have image\n")
    next
  }
  
    download.file(url = img_url,destfile = img_dest,mode = "wb")
    sleep_time<-sample(60:120,size = 1)
    cat("sleeping for ", sleep_time," sec. \n")
    Sys.sleep(sleep_time)
    
    
    }
