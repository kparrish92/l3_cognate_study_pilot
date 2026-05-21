source(here::here("scripts", "00_libraries.R"))


# Read in the data
tidy_data <- fs::dir_ls(here("data", "main_exp"),
                        regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/main_exp/")) %>% 
  mutate(group = "L3_group") %>% 
  filter(!is.na(participant)) %>% 
  select(participant, word, type, correct_reponse, key_resp_lextale_trial.keys, 
         key_resp_lextale_trial.rt, group) %>% 
  filter(!is.na(key_resp_lextale_trial.rt)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% # add correctness
  filter(!is.na(key_resp_lextale_trial.keys)) # remove any na trials


length(unique(tidy_data$participant)) # check total ppts 

totals_per_person = tidy_data %>% # make sure each person has 96 pseudowords + 96 real words
  group_by(participant, type) %>% 
  summarise(n = n())

correct_per_person = tidy_data %>%
  group_by(participant, type, is_correct) %>% 
  summarise(n = n()) %>% 
  filter(is_correct == 1) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(ps_lextale_score = p_lexTALE_score(non_cognate, pseudoword)) %>% 
  select(-is_correct)
## Create Log RTS 
tidy_data$key_resp_lextale_trial.rt = as.numeric(tidy_data$key_resp_lextale_trial.rt)
tidy_data$log_rt = log(tidy_data$key_resp_lextale_trial.rt)

# combine and save data 
tidy_data %>% 
  filter(participant %in% correct_per_person$participant) %>%
  filter(key_resp_lextale_trial.rt < 2) %>% 
  filter(key_resp_lextale_trial.rt > .5) %>% 
  left_join(correct_per_person, by = "participant") %>% 
  write.csv(here("data", "tidy", "tidy_data.csv"))



  
