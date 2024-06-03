library(tidyverse)
library(here)

## Tidy data 

tidy_data <- fs::dir_ls(here("data", "raw"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/raw/")) %>% 
  mutate(source = substr(source, 1, 10)) %>% 
  select(source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0,1,0)) %>% 
  filter(key_resp_lextale_trial.rt < 2 & key_resp_lextale_trial.rt > .5)  %>%  # no trails longer than 2s or less than 500ms
  filter(is_correct == 1) # filter for only correct answers

## Create Log RTS 

tidy_data$key_resp_lextale_trial.rt = as.numeric(tidy_data$key_resp_lextale_trial.rt)
tidy_data$log_rt = log(tidy_data$key_resp_lextale_trial.rt)


## Filter for participants who got more than 70 correct trials 
cdf = tidy_data %>% 
  group_by(source) %>% 
  summarize(no_correct = sum(is_correct)) %>% 
  filter(no_correct > 70) 

unique(cdf$source)

tidy_data %>% 
  filter(source %in% unique(cdf$source)) %>% 
  write.csv(here("data", "tidy", "pilot_data.csv"))
