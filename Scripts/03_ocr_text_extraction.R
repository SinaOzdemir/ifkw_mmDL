## tesseract test
install.packages("tesseract")

library(tesseract)
library(here)
library(tidyverse)
test_data<- list.files(path = here("Data","tesseract_test_data"),pattern = "*.jpg",full.names = T)
img_names<- list.files(path = here("Data","tesseract_test_data"),pattern = "*.jpg",full.names = F)
english<- tesseract("eng")

test_results<- ocr_data(image = test_data[1],
                   engine = english) %>% 
  filter(confidence >80) %>% 
  mutate(image_path = img_names[1])

results <- tibble()
for (i in 1:length(test_data)) {
  img_text<- ocr_data(image = test_data[i],
                      engine = english) %>% 
    filter(confidence > 80) %>% 
    mutate(image_name = img_names[i])
    
  results<- rbind(results,img_text)
}

image_texts<- results %>%
  group_by(image_name) %>% 
  summarise(img_text = paste(word,collapse = " "))
  
saveRDS(image_texts,file = here("Data","tesseract_test_data","img_text_test_res.RDS"))
#works fairly well, could be improved by preprocessing images with magick [https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#Preprocessing_with_Magick]