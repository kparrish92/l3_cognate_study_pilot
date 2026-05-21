## Proficiency stuff

tidy_data_l3 <- fs::dir_ls(here("data", "main_exp"),
                           regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/main_exp/")) %>% 
  mutate(group = "L3_group") %>% 
  filter(!is.na(participant)) %>% 
  select(participant, source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt, group)

tidy_data_l2 <- fs::dir_ls(here("data", "l2_raw"),
                           regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  mutate(source = str_remove(source, "/Users/kyleparrish/Documents/GitHub/l3_cognate_study_pilot/data/l2_raw/")) %>% 
  mutate(group = "L2_group") %>% 
  filter(!is.na(participant)) %>% 
  select(participant, source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt, group)

all_trials = rbind(tidy_data_l2, tidy_data_l3) %>% 
  select(participant, source, word, type, correct_reponse, key_resp_lextale_trial.keys, key_resp_lextale_trial.corr, key_resp_lextale_trial.rt, group) %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_correct =
           ifelse(
             correct_reponse == 1 & key_resp_lextale_trial.keys == 1 | 
               correct_reponse == 0 & key_resp_lextale_trial.keys == 0, 1, 0))


l2_scores = read.csv(here("data", "l2_lex", "cognate_eng_lextale.csv"))
l3_scores = read.csv(here("data", "tidy", "l3_span.csv")) %>% 
  rename(participant = id)


survey = read.csv(here("data", "survey_data", "survey_tidy.csv")) %>% 
  rename(participant = prolific_id)


## 96 non-cognates + 96 pseudowords 

totals_per_person = all_trials %>% 
  group_by(participant, type) %>% 
  summarise(n = n()) %>% 
  filter(type == "non_cognate" | type == "pseudoword")

correct_per_person = all_trials %>% 
  group_by(participant, type, is_correct) %>% 
  summarise(n = n()) %>% 
  filter(is_correct == 1) %>% 
  filter(type == "non_cognate" | type == "pseudoword") %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(ps_lextale_score = lexTALE_score(non_cognate, pseudoword))

lexTALE_score <- function(words_correct, nonwords_correct) {
  ((words_correct / 48 * 100) +
     (nonwords_correct / 96 * 100)) / 2
}


df = l3_scores %>% 
  left_join(correct_per_person, by = "participant") %>% 
  filter(!is.na(ps_lextale_score)) %>% 
  left_join(survey) %>% 
  select(participant, lextale_avg, ps_lextale_score, spanish_speaking_level, spanish_comprehension_level) %>% 
  mutate(
    spanish_speaking_level = as.numeric(str_extract(spanish_speaking_level, "^\\d+"))
  ) %>% 
  mutate(
    spanish_comprehension_level = as.numeric(str_extract(spanish_comprehension_level, "^\\d+"))
  )


lexTALE_score_sp <- function(words_correct, nonwords_correct) {
  ((words_correct / 60 * 100) +
     (nonwords_correct / 30 * 100)) / 2
}


cor.test(df$ps_lextale_score, df$spanish_comprehension_level, method = "pearson")

df_2 = survey %>% 
  left_join(correct_per_person) %>% 
  filter(!is.na(ps_lextale_score)) %>% 
  select(participant, ps_lextale_score, spanish_speaking_level, spanish_comprehension_level) %>% 
  mutate(
    spanish_speaking_level = as.numeric(str_extract(spanish_speaking_level, "^\\d+"))
  ) %>% 
  mutate(
    spanish_comprehension_level = as.numeric(str_extract(spanish_comprehension_level, "^\\d+"))
  )

cor.test(df_2$spanish_comprehension_level, df_2$ps_lextale_score, method = "pearson")


