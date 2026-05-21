freq = read.csv(here("docs", "manuscript", "tables", "stim_detailed.csv")) %>% 
  janitor::clean_names() %>% 
  select(es_wordform, subtlex_es_log10_abs_1) %>% 
  rename(word = es_wordform) %>% 
  rename(frequency = subtlex_es_log10_abs_1) 


tidy_data_l3 <- fs::dir_ls(here("data", "main_exp"),
                           regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% 
  select(key_resp_lextale_trial.rt  , participant, word, type, is_correct) %>% 
  filter(!is.na(is_correct)) %>% 
  left_join(freq, by = "word") %>% 
  filter(!is.na(frequency)) %>% 
  mutate(group = "L3")
  
tidy_data_l2 <- fs::dir_ls(here("data", "l2_raw"),
                           regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0)) %>% 
  select(key_resp_lextale_trial.rt, participant, word, type, is_correct) %>% 
  filter(!is.na(is_correct)) %>% 
  left_join(freq, by = "word") %>% 
  filter(!is.na(frequency)) %>% 
  mutate(group = "L2")

tidy_data_l3 = rbind(tidy_data_l2, tidy_data_l3)


words = tidy_data_l3 %>% group_by(word, is_correct) %>% summarise(n = n())

tidy_data_l3$key_resp_lextale_trial.rt = as.numeric(tidy_data_l3$key_resp_lextale_trial.rt)

tidy_data_l3$log_rt = log(tidy_data_l3$key_resp_lextale_trial.rt)


errors_removed = tidy_data_l3 %>% 
  filter(is_correct == 1) %>% 
  filter(key_resp_lextale_trial.rt < 2) %>% 
  filter(key_resp_lextale_trial.rt >.5) 
  
eligible_qty = errors_removed %>% 
  group_by(participant, type) %>% 
  summarise(n = n()) %>% 
  filter(n < 10)



errors_removed$type = as.factor(errors_removed$type)
errors_removed$type = relevel(errors_removed$type, ref = "two_way_cognate")

model_df = errors_removed %>% 
  filter(group == "L3") %>% 
  filter(!(participant %in% eligible_qty$participant))

l3_model_p = brm(log_rt ~ type + frequency + (1 | word) + (type | participant), data = model_df)

l3_model_f = lmerTest::lmer(log_rt ~ type + frequency + (1 | word) + (type | participant), data = model_df)

conditional_effects(l3_model_p)

describe_posterior(l3_model_p)

summary(l3_model_f)

