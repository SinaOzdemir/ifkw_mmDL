#########################################################
# Title: Data combing for imaged text                   #
# Author: Sina Özdemir                                  #
#         PhD candidate                                 #
#         Department of sociology and political science #
#         NTNU, Norway                                  #
#         sina.ozdemir@ntnu.no                          #
# Date: 05/05/2022                                      #
#########################################################


# Setup -------------------------------------------------------------------

library(pacman)

packs<- c("tidyverse","here","xlsx","rvest")

p_load(char = packs, install = T)

data_path<- here("Data","labelled_data")
eu_data<- readRDS(file = here("Data","source_data","eu_data_011219_310720.RDS"))
coded_sample<- read.xlsx(file = file.path(data_path,"data_PR_plus_2022-06-13_10-21.xlsx"),sheetIndex = 1)


# remove double codings ---------------------------------------------------

coded_sample_duplicated<- coded_sample %>%
  filter(duplicated(V101_01)) %>%
  mutate(STARTED = lubridate::as_datetime(STARTED)) %>% 
  group_by(V101_01) %>% 
  filter(STARTED==max(STARTED))

coded_sample_b<- coded_sample %>%
  filter(!(V101_01%in%coded_sample_duplicated$V101_01)) %>% 
  rbind(.,coded_sample_duplicated)

coded_sample_c<- coded_sample_b %>% select(V101_01,V101_02,V301_02) %>% 
  rename(status_id = V101_01) %>% 
  mutate(V301_02 = recode(V301_02,`2`=0,`1`=1))
# Imaged tweet identification ---------------------------------------------

visual_data<- coded_sample_b%>% 
  filter(!is.na(T001_01))

visual_data_links<- visual_data %>% select(V101_01,V101_02)

# API data ----------------------------------------------------------------

image_data<- eu_data %>% filter(status_id%in%visual_data_links$V101_01)


image_data_links<- image_data %>%
  select(status_id,screen_name,text,media_url) %>% 
  mutate(media_url = as.character(media_url)) %>% 
  filter(!is.na(media_url))


DL_data<-inner_join(coded_sample_c,image_data_links,by = "status_id")

saveRDS(DL_data,file = here("Data","labelled_data","DL_data.RDS"))

