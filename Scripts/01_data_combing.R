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

packs<- c("tidyverse","academictwitteR","renv","here","xlsx")

p_load(char = packs, install = T)

data_path<- here("Data","source_data")

eu_sample<- readRDS(file = file.path(data_path,"eu_data_011219_310720.RDS"))

coded_sample<- read.xlsx(file = file.path(data_path,"data_PR_plus_2022-05-05_15-00.xlsx"),sheetIndex = 1)

coded_sample_duplicated<- coded_sample %>%
  filter(duplicated(V101_01)) %>%
  mutate(STARTED = lubridate::as_datetime(STARTED)) %>% 
  group_by(V101_01) %>% 
  filter(STARTED==max(STARTED))

coded_sample_b<- coded_sample %>%
  filter(!(V101_01%in%coded_sample_duplicated$V101_01)) %>% 
  rbind(.,coded_sample_duplicated)


labelled_id<- coded_sample_b %>% pull(V101_01)


raw_twt_dta<- eu_sample %>%
  filter(status_id%in%labelled_id) %>% 
  select(status_id,screen_name,created_at,text,media_type,media_url)

img_dta<- raw_twt_dta %>%
  filter(media_type == "photo") %>% 
  select(status_id,screen_name,media_url)


DL_data<- coded_sample %>%
  rename(status_id = V101_01) %>% 
  right_join(x = .,y = img_dta, by = "status_id") %>% 
  mutate(media_url = as.character(media_url)) %>% 
  left_join(.,y = raw_twt_dta,by = c("status_id","screen_name")) %>% 
  group_by(status_id) %>% 
  filter(STARTED == max(STARTED)) %>% 
  filter(!duplicated(status_id)) %>% 
  select(status_id,V101_02,screen_name,media_url.x,text,V201,V301_01:V301_06) %>% 
  rename(media_url = media_url.x) %>% 
  mutate(across(V301_01:V301_06,~recode(.x,`2`=1,`1`=0)))

saveRDS(DL_data,file = here("Data","labelled_data","DL_data.RDS"))

write.table(x = DL_data,
            file = here("Data","labelled_data","DL_data.csv"),
            quote = T,sep = ",",row.names = F,
            fileEncoding = "UTF-8")
