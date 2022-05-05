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

dt<- readRDS(file = file.path(data_source,"DL_data.RDS"))



# image scraper -----------------------------------------------------------

##test

test_data<- dt %>%
  select(status_id,media_url) %>% 
  slice_sample(n =5)


#doesn't seem to pick up the image... it could be because of the link
ralger::images_scrap(link = "http://pbs.twimg.com/media/EJKrHGBWoAA1yd0.jpg",imgpath = file.path(data_dest,"test_image"),extn = "jpeg")



a<-images_scrap(link = "https://rstudio.com/",
             imgpath = here("Data"),
             extn = "png")