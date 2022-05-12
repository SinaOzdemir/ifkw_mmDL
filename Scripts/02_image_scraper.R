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

packs<- c("tidyverse","tesseract","here")

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



# OCR on images w text ----------------------------------------------------

## IMPORTANT:: Tesseract vertically processes imgae, so if an image has multiple text clusters, the OCR will confuse and mix it...
## There is probably a better way to handle this with another approach like: 1) identify text clusters 2) recognize the text from image, 3) extract the text
imgs_dr<- list.files(path = data_dest,pattern = "*.jpg",full.names = T)

text_detection<- function(file.path){
  ocr.data<- tesseract::ocr_data(image = file.path,engine = "eng")
  ocr.data$potential = ifelse(ocr.data$confidence >95,1,0)
  
  n.words<- sum(ocr.data$potential)
  if(n.words>=4){
    has_text = T
  }else{
    has_text = F
  }
  
  text_check<- data_frame(image_name = tail(unlist(str_split(string = file.path,pattern = "/",simplify = F)),n = 1),
                          has_text = has_text)
  return(text_check)
}


texted_images<-map_dfr(.x = imgs_dr,.f = text_detection)


text_t_append<- texted_images %>% filter(has_text == T) %>% pull(image_name)

txt_img_dr<- imgs_dr[grepl(pattern = paste(text_t_append,collapse = "|"),x = imgs_dr,fixed = F)]


e<- a[stringi::stri_enc_isascii(a)]


install.packages("magick")
library(magick)

#magic_test

a<- tesseract::ocr_data(image = imgs_dr[7],engine="eng") %>% 
  filter(confidence>85) %>% 
  summarise(text = paste(word,collapse=" ")) %>% 
  pull(text)

b<- image_read(path = imgs_dr[7]) %>%
  image_resize("2000x") %>% #not sure what this means, just using the example from https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html
  image_convert(type = "Grayscale") %>% 
  image_trim(fuzz = 50) %>% 
  image_write(format = "jpg",density = "300x300") %>% 
  tesseract::ocr_data() %>% 
  filter(confidence>85) %>% 
  summarise(text = paste(word,collapse=" ")) %>% 
  pull(text)
  

a
b
