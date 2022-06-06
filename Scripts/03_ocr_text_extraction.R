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

ocr_image <- test_img %>% 
  image_resize(geometry = "1000x") %>% 
  image_convert(type = "bilevel") %>% 
  image_reducenoise() %>% 
  image_negate()


#based on these experiments here is the suggested preprocessing for images

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



# function test ----------------------------------------------------------

text_extractor<- function(image_path,preprocess = c(T,F),postprocess = c(T,F),text_confidence = numeric(0),OCR_language = c("eng","osd")){
  
  #make sure the user has necessary dependencies
  p_loader<- require(pacman)
  
  if(isFALSE(p_loader)){
    install.packages("pacman")
  }else{
    library(pacman)
  }
  
  packs<- c("tidyverse","tesseract","magick")
  
  p_load(char = packs, install = T)
  
  eng = tesseract(language = OCR_language)
  
  image = image_read(path = image_path)
  img_name = tail(unlist(str_split(string = image_path,pattern = "/")),n = 1)
  
  if(isTRUE(preprocess)){
    message("preprocessing the image")
    image = image %>% 
      image_resize(geometry = "1000x") %>% 
      image_convert(type = "grayscale") %>% 
      image_reducenoise() %>% 
      image_negate()
  }
  
  
  message("extracting text from the image\n")
  
  image_text<- ocr_data(image = image,engine = eng) %>% 
    filter(confidence >= text_confidence) %>% 
    mutate(image_name = img_name) %>% 
    group_by(image_name) %>% 
    summarise(text = paste0(word,collapse = " "))
  
  if(isTRUE(postprocess)){
    message("post-processing the extracted text\n")
    post_processed_image_text<- image_text %>%
      mutate(text = trimws(text)) %>% 
      mutate(text = str_remove_all(string = text, pattern = "[[:symbol:]]")) %>% 
      mutate(text = str_remove_all(string = text, pattern = "\\\\")) %>% 
      mutate(text = str_remove_all(string = text,pattern = "[[:punct:]]")) %>% 
      mutate(text = str_remove_all(string = text, pattern = "[^[\\da-zA-Z ]]")) %>% 
      mutate(text = str_replace_all(string = text, pattern = "\\s+",replacement = " ")) %>% 
      mutate(text = trimws(x = text)) %>% 
      filter(nchar(text)>1)
    
    if(nrow(post_processed_image_text)>0){
      
      message("post-processing completed returning values\n")
      return(post_processed_image_text)
      
    }else{
      
      message("post-processing removed all values, returning null\n")
      return(NULL)
      
    }
  }else{
    message("post-processign was not chosen, returning raw data")
    return(image_texts)
  }
  
  
  
}
#there is still some noise but works relatively fine
function_test<- map_dfr(.x = test_data,.f = text_extractor,preprocess = T, text_confidence = 75,OCR_language = "eng",postprocess = T)

## OCR returns quite noise, some post-processing is needed
###further text cleaning for ocr
cleaning_test<- image_texts

cleaned_text<- cleaning_test %>%
  mutate(text = trimws(text)) %>% 
  mutate(text = str_remove_all(string = text, pattern = "[[:symbol:]]")) %>% 
  mutate(text = str_remove_all(string = text, pattern = "\\\\")) %>% 
  mutate(text = str_remove_all(string = text,pattern = "[[:punct:]]")) %>% 
  mutate(text = str_remove_all(string = text, pattern = "[^[\\da-zA-Z ]]")) %>% 
  mutate(text = str_replace_all(string = text, pattern = "\\s+",replacement = " ")) %>% 
  mutate(text = trimws(x = text)) %>% 
  filter(nchar(text)>1)

### integrated into the function now

# implementation ----------------------------------------------------------

labelled_images<- list.files(path = here("Data","image_data"),pattern = "*.jpg",full.names = T)

image_texts<- map_dfr(.x = labelled_images,.f = text_extractor,preprocess = T,text_confidence = 75,OCR_language = "eng",postprocess = T)

saveRDS(object = image_texts,file = here("Data","shallow_learning_data","image_texts.rds"))
