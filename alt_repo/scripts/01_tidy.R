library(tidyverse)
library(here)


# Step 1: Read in the data
tidy_data_l3 <- fs::dir_ls(here("data", "main_exp"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/main_exp/")) %>% 
  mutate(group = "L3_group")


# Step 1: Read in the data
tidy_data_l2 <- fs::dir_ls(here("data", "l2_raw"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/l2_raw/")) %>% 
  mutate(group = "L2_group")


tidy_data = rbind(tidy_data_l3, tidy_data_l2)


# Check the final dataframe
print(tidy_data)

# Step 3: Merge the unique IDs back to the main dataframe
tidy_data <- tidy_data %>%
  select(participant, source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt, group) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% 
  filter(key_resp_lextale_trial.rt < 2 & key_resp_lextale_trial.rt > 0.5) 


## Create Log RTS 

tidy_data$key_resp_lextale_trial.rt = as.numeric(tidy_data$key_resp_lextale_trial.rt)
tidy_data$log_rt = log(tidy_data$key_resp_lextale_trial.rt)

tidy_data = tidy_data %>% 
  filter(key_resp_lextale_trial.rt < 2 & key_resp_lextale_trial.rt > 0.5) 

## Filter for participants who got more than 50 correct trials 
cdf = tidy_data %>% 
  group_by(participant) %>% 
  summarize(no_correct = sum(is_correct)) %>% 
  filter(no_correct > 50) 


cdf_f = tidy_data %>% 
  filter(participant %in% unique(cdf$participant)) %>% 
  group_by(participant, type) %>% 
  summarize(qty = n()) %>% 
  filter(qty > 8)

tidy_data %>% 
  filter(participant %in% unique(cdf$participant)) %>% 
  filter(participant %in% unique(cdf_f$participant)) %>% 
  write.csv(here("data", "tidy", "pilot_data.csv"))
