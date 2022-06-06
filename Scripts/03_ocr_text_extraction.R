########################################
#Title: Text extraction from images    #
#Author: Sina Özdemir                  #
#        PhD candidate                 #
#        sina.ozdemir@ntnu.no          #
#        Dep. of Socio. and PoliSci    #
#        NTNU                          #
########################################




# setup -------------------------------------------------------------------
if (isFALSE(require(pacman))){
  install.packages("pacman")
  library(pacman)
}else{
  library(pacman)
}

packs<- c("tidyverse","here","tesseract","magick")  
  
p_load(char = packs)


# data --------------------------------------------------------------------

##image data

test_data<- list.files(path = here("Data","tesseract_test_data"),pattern = "*.jpg",full.names = T)
img_names<- list.files(path = here("Data","tesseract_test_data"),pattern = "*.jpg",full.names = F)


# tests -------------------------------------------------------------------

## text extraction test

english<- tesseract("eng")

test_results<- ocr_data(image = test_data[1],
                   engine = english) %>% 
  filter(confidence >75) %>% 
  mutate(image_path = img_names[1]) %>% 
  group_by(image_path) %>% 
  summarise(text = paste0(word,collapse = " "))

## image preprocessing test

test_img<- image_read(path = test_data[1])

resize_test_image<- test_img %>% 
  image_resize(image = .,geometry = "2000x")#resizes the image to NNNx pixel

conv_image<- resize_test_image %>% 
  image_convert(image = .,
                format = , #determines the output format (jpg,png etc.)
                type = "bilevel",# determines the image type (grayscale,monochrome etc.) https://www.imagemagick.org/Magick++/Enumerations.html#ImageType
                colorspace = , #color palate for modification https://www.imagemagick.org/Magick++/Enumerations.html#ColorspaceType
                depth = , # color depth (8 or 16) not sure what this mean except I had this option on my windows 3.1 machine :D
                antialias = ,#enable anti-aliasing for text and strokes. Not sure what this does either, I always turn this off in games.
                matte = )#logical for transparency

#type = "bilevel" turns the image black and white, type = "grayscale" turns it gray.
#Bilevel might improve text extraction better than gray scale

noise_reduce<- conv_image %>% 
  image_reducenoise()#automatic noise reduction, doesn't do much

trimmed_image<- noise_reduce %>% 
  image_trim(fuzz = 10)# crops out whitespace in the margins, most of our images don't have such thing

trimmed_image %>% image_negate()#negation reverses the color order might be useful because black on white is easier to detect

# text detection can be further improved by applying convolve kernels but there seems to a lot of different approaches to edge detection.
# So it would take some trial and error to find the optimum kernel since we have wide variety of them 

#based on these experiments here is the suggested preprocessing for images

ocr_image <- test_img %>% 
  image_resize(geometry = "1000x") %>% 
  image_convert(type = "bilevel") %>% 
  image_reducenoise() %>% 
  image_negate()

ocr_image_a <- test_img %>% 
  image_resize(geometry = "1000x") %>% 
  image_convert(type = "grayscale") %>% 
  image_reducenoise() %>% 
  image_negate()

pp_image_ocr<- ocr_data(image = ocr_image,engine = english) %>% 
  filter(confidence> 75) %>% 
  mutate(image_name = img_names[1]) %>% 
  group_by(image_name) %>% 
  summarise(text = paste0(word,collapse = " "))

pp_image_ocr_a<- ocr_data(image = ocr_image_a,engine = english) %>% 
  filter(confidence> 75) %>% 
  mutate(image_name = img_names[1]) %>% 
  group_by(image_name) %>% 
  summarise(text = paste0(word,collapse = " "))



test_results$text # = -50 minutes #BERECpublic
pp_image_ocr$text # - 30 minutes
pp_image_ocr_a$text # -30 minutes #BERECpublic
original_text<- "-30 minutes #BERECpublic"

stringdist::stringdist(a = original_text,b = test_results$text, method = "lv")#need to change 5 characters to make b same as a

stringdist::stringdist(a = original_text,b = pp_image_ocr$text, method = "lv")#need to change 13 characters to make b same as a

stringdist::stringdist(a = original_text,b = pp_image_ocr_a$text, method = "lv")#perfect match

#preprocessing option 3 increases the OCR accuracy.



# Implementation ----------------------------------------------------------

images_list
