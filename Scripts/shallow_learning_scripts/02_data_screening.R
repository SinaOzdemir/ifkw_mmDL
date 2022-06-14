##########################################################
# Title:Data screening for shallow learning(to Appendix) #
# Author: Sina Özdemir                                   #
#         PhD candidate                                  #
#         Department of Sociology and political science  #
#         Norwegian university of science and tech.(NTNU)#
##########################################################


# setup -------------------------------------------------------------------


require(pacman)

packs<- c("tidyverse","here","caret","feather")

p_load(char = packs, install = T)

outcome_variables<- readRDS(file = here("Data","labelled_data","DL_data.RDS"))

predictor_variables<- readRDS(file = here("Data","shallow_learning_data","hot_encoding_dfm.rds"))
  


outcome_variables<- outcome_variables %>% 
  ungroup() %>% 
  mutate(doc_id = paste0(screen_name,"-",status_id)) %>% 
  select(doc_id,V301_02) %>%
  filter(!duplicated(doc_id)) %>% 
  drop_na()

predictor_variables<- predictor_variables %>% 
  rownames_to_column(var = "doc_id")

ml_data<- inner_join(outcome_variables,predictor_variables, by = "doc_id")


# zero variance -----------------------------------------------------------

pd_nzv<- nearZeroVar(x = predictor_variables[,2:2735],saveMetrics = T,names = T,foreach = T,allowParallel = T)

pd_nzv_sum<- pd_nzv %>% 
  group_by(nzv) %>% 
  tally() %>% 
  mutate(perc = n/nrow(pd_nzv)) %>% 
  mutate(test_stat = "NearZeroVar") %>% 
  rename(logical = "nzv")

pd_zv_sum<- pd_nzv %>% 
  group_by(zeroVar) %>% 
  tally() %>% 
  mutate(perc = n/nrow(pd_nzv)) %>% 
  mutate(test_stat = "zerovar") %>% 
  rename(logical = "zeroVar")

variance_check<- rbind(pd_nzv_sum,pd_zv_sum)

var_graph<- variance_check %>%
  ggplot(aes(x = logical, y = perc, group = test_stat))+
  geom_bar(aes(fill = test_stat),stat = "identity",position="dodge")+
  theme_bw()+
  labs(x = "",y = "Percentage of predictors",subtitle = "N of predictors = 2735")+
  guides(fill = guide_legend(title ="Variance test statistic"))

ggsave(var_graph, filename = "variance_check.jpg",path = here("Graphs"),bg = "white")

saveRDS(ml_data,file = here("Data","shallow_learning_data","hot_encoding_ml.rds"))
