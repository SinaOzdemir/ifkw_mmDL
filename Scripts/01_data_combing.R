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

coded_sample<- read.xlsx(file = file.path(data_path,"data_PR_plus_2022-05-05_15-00.xlsx"),sheetIndex = 1) %>% 
  select(V101_01,V101_02,V201,V301_01:V401_12) %>% 
  mutate(across(V301_01:V401_12,~ifelse(.x == 2,1,0))) %>% 
  drop_na()
  

labelled_id<- coded_sample %>% pull(V101_01)


raw_twt_dta<- eu_sample %>%
  filter(status_id%in%labelled_id)

img_dta<- raw_twt_dta %>%
  filter(media_type == "photo") %>% 
  select(status_id,screen_name,media_url)


DL_data<- coded_sample %>%
  rename(status_id = V101_01) %>% 
  right_join(x = .,y = img_dta, by = "status_id") %>% 
  mutate(media_url = as.character(media_url))

saveRDS(DL_data,file = here("Data","labelled_data","DL_data.RDS"))

write.table(x = DL_data,
            file = here("Data","labelled_data","DL_data.csv"),
            quote = T,sep = ",",row.names = F,
            fileEncoding = "UTF-8")
