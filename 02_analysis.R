

ppts_remove = c("6789000b08f2703ba0b959b1")

p_data = read.csv(here("data", "tidy", "tidy_data.csv")) %>% 
  filter(!is.na(participant)) %>% 
  filter(key_resp_lextale_trial.rt < 2) %>% 
  filter(key_resp_lextale_trial.rt > .5) %>% 
  filter(!participant %in% ppts_remove)

outlier_ppt = read.csv(here("data", "tidy", "tidy_data.csv")) %>% 
  filter(participant %in% ppts_remove)

outlier_ppt %>% ## outlier info if we want to talk about this person - 33
  group_by(type, is_correct) %>% 
  summarise(n = n()) %>% 
  filter(is_correct == 1)




#### Accuracy

totals = p_data %>% # create denominator for accuracy
  group_by(type) %>% 
  summarise(n = n())

totals_n = p_data %>% # create numerator for accuracy
  group_by(type, is_correct) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = is_correct, values_from = n) %>% 
  rename("correct" = "1") %>% 
  rename("incorrect" = "0") %>% 
  mutate(total = correct + incorrect) %>% 
  mutate(pct_correct = correct/total)

p_data %>% 
  group_by(participant, type, is_correct) %>% 
  summarize(n = n())





#### Frequentist model
l3_data_p = p_data %>% filter(group == "L3_group") %>% 
  filter(is_correct == 1) %>%
  filter(type != "pseudoword")


l3_data_p$type = as.factor(l3_data_p$type)
l3_data_p$type = relevel(l3_data_p$type, ref = "two_way_cognate")


l3_model_p = lmerTest::lmer(log_rt ~ type + ps_lextale_score + (1 | word) + (type | participant), data = l3_data_p)


summary(l3_model_p)
