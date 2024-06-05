library(tidyverse)
library(here)

# Step 1: Read in the data
tidy_data <- fs::dir_ls(here("data", "raw"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) 
# Step 2: Create a unique participant ID for each source
unique_sources <- tidy_data %>% distinct(source) %>%
  mutate(ppt = row_number())

# Step 3: Merge the unique IDs back to the main dataframe
tidy_data <- tidy_data %>%
  left_join(unique_sources, by = "source") %>%
  select(ppt, source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% 
  filter(key_resp_lextale_trial.rt < 2 & key_resp_lextale_trial.rt > 0.3)  %>%  # no trials longer than 2s or less than 300ms
  filter(is_correct == 1) # filter for only correct answers %>% 

# Check the final dataframe
print(tidy_data)

## Create Log RTS 

tidy_data$key_resp_lextale_trial.rt = as.numeric(tidy_data$key_resp_lextale_trial.rt)
tidy_data$log_rt = log(tidy_data$key_resp_lextale_trial.rt)

tidy_data = tidy_data %>% 
  filter(key_resp_lextale_trial.rt < 2 & key_resp_lextale_trial.rt > 0.3) 

## Filter for participants who got more than 70 correct trials 
cdf = tidy_data %>% 
  group_by(ppt) %>% 
  summarize(no_correct = sum(is_correct)) %>% 
  filter(no_correct > 50) 


tidy_data %>% 
  filter(ppt %in% unique(cdf$ppt)) %>% 
  write.csv(here("data", "tidy", "pilot_data.csv"))
